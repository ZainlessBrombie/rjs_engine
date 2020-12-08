use crate::js::data::engine_constants::ConstantStrings;
use crate::js::data::js_execution::{native_from, EngineQueuer, FnOpRepr, JsVar, VarAlloc};
use crate::js::data::js_types::{Identity, JSCallable, JsFn, JsObj, JsProperty, JsValue};
use safe_gc::{Gc, GcCell};
use std::borrow::BorrowMut;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::os::raw::c_void;
use std::rc::Rc;

pub struct JsObjectBuilder<'a> {
    obj: JsObj,
    strings: Option<&'a ConstantStrings>,
}

impl<'a> JsObjectBuilder<'a> {
    pub fn new(constants: Option<&ConstantStrings>) -> JsObjectBuilder {
        return JsObjectBuilder {
            obj: JsObj {
                is_array: false,
                content: Default::default(),
                symbol_keys: Default::default(),
                call: JSCallable::NotCallable,
                identity: Identity::new(),
                is_symbol: false,
            },
            strings: constants,
        };
    }

    pub fn with_prop(self, key: Rc<String>, value: JsValue) -> Self {
        return self.with_prop_full(key, value, true, true, true);
    }

    pub fn with_prop_full(
        mut self,
        key: Rc<String>,
        value: JsValue,
        enumerable: bool,
        configurable: bool,
        writable: bool,
    ) -> Self {
        self.obj.content.insert(
            key,
            JsProperty {
                enumerable,
                configurable,
                writable,
                value,
            },
        );
        self
    }

    pub fn with_proto(self, proto: JsValue) -> Self {
        let proto_str = self
            .strings
            .map(|s| s.proto.clone())
            .unwrap_or_else(|| Rc::new("__proto__".into()));
        return self.with_prop_full(proto_str, proto, false, false, false);
    }

    pub fn with_being_symbol(mut self) -> Self {
        self.obj.is_symbol = true;
        self
    }

    pub fn with_callable(mut self, callable: JSCallable) -> Self {
        self.obj.call = callable;
        return self;
    }

    pub fn with_being_array(mut self) -> Self {
        self.obj.is_array = true;
        return self;
    }

    pub fn with_symbol_full(
        mut self,
        key: JsValue,
        value: JsValue,
        enumerable: bool,
        configurable: bool,
        writable: bool,
    ) -> Self {
        self.obj.symbol_keys.insert(
            key,
            JsProperty {
                enumerable,
                configurable,
                writable,
                value,
            },
        );
        return self;
    }

    pub fn build(self) -> JsValue {
        return JsValue::Object(Gc::new(GcCell::new(self.obj)));
    }
}

pub fn u_null() -> JsValue {
    JsValue::Null
}

pub fn u_undefined() -> JsValue {
    JsValue::Undefined
}

pub fn u_number(n: f64) -> JsValue {
    return JsValue::Number(n);
}

pub fn u_bool(b: bool) -> JsValue {
    return JsValue::Boolean(b);
}

pub fn u_false() -> JsValue {
    u_bool(false)
}

pub fn u_true() -> JsValue {
    u_bool(true)
}

pub fn u_string(s: &str) -> JsValue {
    return JsValue::String(Rc::new(s.into()));
}

thread_local! {
    static STR_POOL: RefCell<HashMap<usize, Rc<String>>> = RefCell::new(Default::default());
}

pub fn s_pool(s: &'static str) -> Rc<String> {
    STR_POOL.with(|map| {
        match map
            .borrow_mut()
            .entry(s as *const str as *const c_void as usize)
        {
            Entry::Occupied(o) => o.get().clone(),
            Entry::Vacant(o) => {
                let ret = Rc::new(s.to_string());
                o.insert(ret.clone());
                ret
            }
        }
    })
}

type Scope = Rc<RefCell<ScopeLookup>>;

type Sl = ScopeLookup;

pub struct ScopeLookup {
    cur: HashMap<Rc<String>, VarAlloc>,
    prev: Option<Scope>,
    local_counter: usize,
    heap_break: bool,
}

impl ScopeLookup {
    pub fn new() -> Scope {
        Rc::new(RefCell::new(ScopeLookup {
            cur: Default::default(),
            prev: None,
            local_counter: 2,
            heap_break: false,
        }))
    }

    pub fn child(this: Scope, heap_break: bool) -> Scope {
        Rc::new(RefCell::new(ScopeLookup {
            cur: Default::default(),
            prev: Some(this.clone()),
            local_counter: 2,
            heap_break,
        }))
    }

    pub fn get_captures(&self) -> Vec<VarAlloc> {
        let mut ret = Vec::new();
        for val in self.cur.values() {
            if let VarAlloc::CapturedAt { .. } = val {
                ret.push(val.clone())
            }
        }
        return ret;
    }

    pub fn get(this: &Scope, name: &Rc<String>, heaped: bool) -> Option<VarAlloc> {
        let ret = RefCell::borrow_mut(this).cur.get(name).map(|r| {
            if heaped {
                return VarAlloc::CapturedAt {
                    name: name.clone(),
                    from: Box::new(r.clone()),
                    target: 0,
                };
            }
            r.clone()
        });
        let mut ret = ret.or_else(|| {
            let this = RefCell::borrow(this);
            this.prev.as_ref().map_or(None, |p| {
                if heaped {
                    Sl::get(p, name, false).map(|source| {
                        return VarAlloc::CapturedAt {
                            name: name.clone(),
                            from: Box::new(source),
                            target: 0,
                        };
                    })
                } else {
                    ScopeLookup::get(&p, &name, this.heap_break)
                    // TODO possibly incorrect
                }
            })
        });
        if let Some(ret) = &mut ret {
            if let VarAlloc::CapturedAt { target, .. } = ret {
                let mut this = RefCell::borrow_mut(this);
                this.local_counter += 1;
                *target = this.local_counter - 1;
            }
        }
        return ret;
    }

    pub fn insert_here(this: &Scope, name: Rc<String>) -> VarAlloc {
        return ScopeLookup::get(&this, &name, false).unwrap_or_else(|| {
            let mut this = RefCell::borrow_mut(this);
            let ret = VarAlloc::LocalAt(name.clone(), this.local_counter);
            this.local_counter += 1;
            this.cur.insert(name.clone(), ret.clone());
            ret
        });
    }

    pub fn get_or_global(this: &Scope, name: Rc<String>) -> VarAlloc {
        return ScopeLookup::get(this, &name, false)
            .unwrap_or_else(|| ScopeLookup::insert_top(this, name));
    }

    pub fn insert_top(this: &Scope, name: Rc<String>) -> VarAlloc {
        let mut borrow1 = RefCell::borrow_mut(this);
        if let Some(prev) = &borrow1.prev {
            return ScopeLookup::insert_top(prev, name);
        } else {
            let v = VarAlloc::Static(name.clone(), JsVar::new(name.clone()));
            (&mut borrow1).cur.insert(name.clone(), v.clone());
            return v;
        }
    }

    pub fn temp_var(this: &Scope) -> VarAlloc {
        let mut this = borm(this);
        this.local_counter += 2;
        return VarAlloc::LocalAt(Rc::new("".into()), this.local_counter - 1);
    }
}

pub struct OpBuilder {
    scopes: Scope,
    statements: Vec<FnOpRepr>,
}
fn borm(s: &Scope) -> RefMut<ScopeLookup> {
    RefCell::borrow_mut(s)
}
fn bor(s: &Scope) -> Ref<ScopeLookup> {
    RefCell::borrow(s)
}
impl OpBuilder {
    pub fn start() -> OpBuilder {
        OpBuilder {
            scopes: ScopeLookup::new(),
            statements: vec![],
        }
    }

    fn sub_b(&mut self) -> OpBuilder {
        OpBuilder {
            scopes: self.scopes.clone(),
            statements: vec![],
        }
    }

    fn scope(&mut self, heap_break: bool) -> OpBuilder {
        OpBuilder {
            scopes: Sl::child(self.scopes.clone(), heap_break),
            statements: vec![],
        }
    }

    pub fn var_dec(&mut self, _t: VType, name: Rc<String>) -> &mut Self {
        ScopeLookup::insert_here(&self.scopes, name.clone());
        self
    }

    pub fn var_r(&mut self, name: Rc<String>) -> &mut Self {
        self.statements.push(FnOpRepr::ReadVar {
            which: Sl::get_or_global(&self.scopes, name),
        });
        self
    }

    pub fn var_w(&mut self, name: Rc<String>, what: impl FnOnce(&mut OpBuilder)) -> &mut OpBuilder {
        let mut result = self.sub_b();
        what(&mut result);
        self.statements.push(FnOpRepr::Assign {
            target: Sl::get_or_global(&self.scopes, name),
            what: Rc::new(FnOpRepr::Multi {
                block: result.statements,
            }),
        });
        self
    }

    pub fn var_w_t(&mut self, v: VarAlloc, what: impl FnOnce(&mut OpBuilder)) -> &mut OpBuilder {
        let mut result = self.sub_b();
        what(&mut result);
        self.statements.push(FnOpRepr::Assign {
            target: v,
            what: Rc::new(FnOpRepr::Multi {
                block: result.statements,
            }),
        });
        self
    }

    pub fn var_r_t(&mut self, v: VarAlloc) {
        self.statements.push(FnOpRepr::ReadVar { which: v })
    }

    pub fn block(&mut self, builder: impl FnOnce(&mut OpBuilder)) -> &mut Self {
        let mut block = OpBuilder {
            scopes: ScopeLookup::child(self.scopes.clone(), false),
            statements: vec![],
        };
        builder(&mut block);
        self.statements.append(&mut block.statements);
        self
    }

    pub fn ifb(
        &mut self,
        condition: impl FnOnce(&mut OpBuilder),
        if_block: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        self.if_elseb(condition, if_block, |_| {});
        self
    }

    pub fn if_elseb(
        &mut self,
        condition: impl FnOnce(&mut OpBuilder),
        if_block: impl FnOnce(&mut OpBuilder),
        else_block: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        let mut c_b = self.sub_b();
        let mut i_b = self.scope(false);
        let mut e_b = self.scope(false);

        condition(&mut c_b);
        if_block(&mut i_b);
        else_block(&mut e_b);

        self.statements.push(FnOpRepr::IfElse {
            condition: Rc::new(FnOpRepr::Multi {
                block: c_b.statements,
            }),
            if_block: Rc::new(FnOpRepr::Multi {
                block: i_b.statements,
            }),
            else_block: Rc::from(FnOpRepr::Multi {
                block: e_b.statements,
            }),
        });

        self
    }

    pub fn ret(&mut self, what: impl FnOnce(&mut OpBuilder)) -> &mut OpBuilder {
        let mut result = self.sub_b();
        what(&mut result);
        self.statements.push(FnOpRepr::Return {
            what: Rc::from(FnOpRepr::Multi {
                block: result.statements,
            }),
        });
        self
    }

    pub fn static_v(&mut self, what: JsValue) -> &mut OpBuilder {
        self.statements.push(FnOpRepr::LoadStatic { value: what });
        self
    }

    pub fn throw(&mut self, what: impl FnOnce(&mut OpBuilder)) -> &mut OpBuilder {
        let mut result = self.sub_b();
        what(&mut result);
        self.statements.push(FnOpRepr::Throw {
            what: Rc::new(FnOpRepr::Multi {
                block: result.statements,
            }),
        });

        self
    }

    pub fn while_l(
        &mut self,
        condition: impl FnOnce(&mut OpBuilder),
        body: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        let mut cond_r = self.sub_b();
        let mut body_r = self.scope(false);

        condition(&mut cond_r);
        body(&mut body_r);

        self.statements.push(FnOpRepr::While {
            condition: Rc::from(FnOpRepr::Multi {
                block: cond_r.statements,
            }),
            block: Rc::from(FnOpRepr::Multi {
                block: body_r.statements,
            }),
        });

        self
    }

    pub fn for_l(
        &mut self,
        init: impl FnOnce(&mut OpBuilder),
        condition: impl FnOnce(&mut OpBuilder),
        each: impl FnOnce(&mut OpBuilder),
        body: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        let mut parent = self.scope(false); // Used to get a scope
        let mut init_r = parent.sub_b();
        let mut cond_r = parent.sub_b();
        let mut each_r = parent.sub_b();
        let mut body_r = parent.sub_b();

        init(&mut init_r);
        condition(&mut cond_r);
        each(&mut each_r);
        body(&mut body_r);

        self.statements.push(FnOpRepr::For {
            initial: Rc::new(FnOpRepr::Multi {
                block: init_r.statements,
            }),
            condition: Rc::new(FnOpRepr::Multi {
                block: cond_r.statements,
            }),
            each: Rc::new(FnOpRepr::Multi {
                block: each_r.statements,
            }),
            block: Rc::new(FnOpRepr::Multi {
                block: body_r.statements,
            }),
        });
        self
    }

    pub fn literal(&mut self, val: JsValue) -> &mut OpBuilder {
        self.statements.push(FnOpRepr::LoadStatic { value: val });
        self
    }

    pub fn func(&mut self, body: impl FnOnce(&mut OpBuilder)) -> &mut OpBuilder {
        let mut f = self.scope(true);
        body(&mut f);
        self.statements.push(FnOpRepr::InstantiateFunction {
            vars: Rc::new(bor(&f.scopes).get_captures()),
            code: Rc::new(FnOpRepr::Multi {
                block: f.statements,
            }),
        });
        self
    }

    pub fn obj_e(&mut self) -> &mut OpBuilder {
        self.statements
            .push(FnOpRepr::NewObject { is_array: false });
        self
    }

    pub fn array_e(&mut self) -> &mut OpBuilder {
        let t_v = self.var_t();
        self.statements.push(FnOpRepr::Assign {
            target: t_v.clone(),
            what: Rc::from(FnOpRepr::NewObject { is_array: true }),
        });
        self.assign_ref(
            |b| b.var_r_t(t_v.clone()),
            |b| {
                b.literal(u_string("length"));
            },
            |b| {
                b.literal(u_number(0.0));
            },
        );
        self.assign_ref(
            |b| b.var_r_t(t_v.clone()),
            |b| {
                b.literal(u_string("__proto__"));
            },
            |b| {
                b.var_r(s_pool("Array")); // TODO true global
            },
        );
        self.var_r_t(t_v);

        self
    }

    pub fn numeral_add(
        &mut self,
        left: impl FnOnce(&mut OpBuilder),
        right: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        let mut lb = self.sub_b();
        let mut rb = self.sub_b();

        left(&mut lb);
        right(&mut rb);

        self.statements.push(FnOpRepr::NumeralPlus {
            left: Rc::new(FnOpRepr::Multi {
                block: lb.statements,
            }),
            right: Rc::new(FnOpRepr::Multi {
                block: rb.statements,
            }),
        });

        self
    }

    pub fn assign_ref(
        &mut self,
        to: impl FnOnce(&mut OpBuilder),
        key: impl FnOnce(&mut OpBuilder),
        what: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        let mut to_b = self.sub_b();
        let mut key_b = self.sub_b();
        let mut what_b = self.sub_b();

        to(&mut to_b);
        key(&mut key_b);
        what(&mut what_b);

        self.statements.push(FnOpRepr::AssignRef {
            to: Rc::new(FnOpRepr::Multi {
                block: to_b.statements,
            }),
            key: Rc::new(FnOpRepr::Multi {
                block: key_b.statements,
            }),
            what: Rc::new(FnOpRepr::Multi {
                block: what_b.statements,
            }),
        });

        self
    }

    pub fn var_t(&mut self) -> VarAlloc {
        Sl::temp_var(&self.scopes) // TODO check that the local var is used locally only
    }

    pub fn load_global(&mut self, name: Rc<String>) {
        self.statements.push(FnOpRepr::LoadGlobal { name })
    }

    pub fn deref(
        &mut self,
        value: impl FnOnce(&mut OpBuilder),
        key: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        let mut k = self.sub_b();
        let mut v = self.sub_b();
        key(&mut k);
        value(&mut v);

        self.statements.push(FnOpRepr::Deref {
            from: Rc::new(FnOpRepr::Multi {
                block: v.statements,
            }),
            key: Rc::new(FnOpRepr::Multi {
                block: k.statements,
            }),
        });
        self
    }

    pub fn call(
        &mut self,
        this: VarAlloc,
        on: impl FnOnce(&mut OpBuilder),
        args: impl FnOnce(&mut OpBuilder),
    ) -> &mut OpBuilder {
        let mut on_b = self.sub_b();
        let mut args_b = self.sub_b();

        on(&mut on_b);
        args(&mut args_b);

        self.statements.push(FnOpRepr::CallFunction {
            this,
            on: Rc::new(FnOpRepr::Multi {
                block: on_b.statements,
            }),
            arg_array: Rc::new(FnOpRepr::Multi {
                block: args_b.statements,
            }),
        });
        self
    }

    pub fn this(&mut self) -> VarAlloc {
        VarAlloc::LocalAt(Rc::new("".into()), 0)
    }

    pub fn args(&mut self) -> VarAlloc {
        VarAlloc::LocalAt(Rc::new("".into()), 1)
    }

    pub fn build(self) -> JsValue {
        JsObjectBuilder::new(None)
            .with_callable(JSCallable::Js {
                content: Rc::new("".to_string()),
                creator: Gc::new(JsFn {
                    ops: Rc::new(FnOpRepr::Multi {
                        block: self.statements,
                    }),
                    captures: Rc::new(vec![]),
                }),
            })
            .build()
    }
}

#[macro_export]
macro_rules! js_primitive {
    ($what:ident) => {{
        trait ToJs {
            fn to_js(&self) -> JsValue;
        }
        impl ToJs for String {
            fn to_js(&self) -> JsValue {
                u_string(self)
            }
        }
        impl ToJs for &str {
            fn to_js(&self) -> JsValue {
                u_string(self)
            }
        }
        impl ToJs for f64 {
            fn to_js(&self) -> JsValue {
                u_number(*self)
            }
        }
        ToJs::to_js(&$what)
    }};
    ($what:literal) => {{
        trait ToJs {
            fn to_js(&self) -> JsValue;
        }
        impl ToJs for String {
            fn to_js(&self) -> JsValue {
                u_string(self)
            }
        }
        impl ToJs for &str {
            fn to_js(&self) -> JsValue {
                u_string(self)
            }
        }
        impl ToJs for f64 {
            fn to_js(&self) -> JsValue {
                u_number(*self)
            }
        }
        ToJs::to_js(&$what)
    }};
}

#[macro_export]
macro_rules! prim_to_val {
    ($what:ident) => {
        (|b: &mut OpBuilder| b.literal(js_primitive!($what)))
    };
    ($what:literal) => {
        (|b: &mut OpBuilder| {
            b.literal(js_primitive!($what));
        })
    };
    ($what:expr) => {
        $what
    };
}

#[macro_export]
macro_rules! js_prop {
    (($from:expr)$([$key:expr])+[$key2:expr]) => {
        (|b: &mut OpBuilder| {
            b.deref(|b| {
                js_prop!((prim_to_val!($from)(b))$([$key])+)(b);
            }, |b| {
                prim_to_val!($key2)(b);
            });
        })
    };
    (($from:expr)[$key2:expr]) => {
        (|b: &mut OpBuilder| {
            b.deref(|b| {
                prim_to_val!($from)(b);
            }, |b| {
                prim_to_val!($key2)(b);
            });
        })
    }
}

#[macro_export]
macro_rules! js_val {
    (undefined) => {(|b: &mut OpBuilder| {
        b.literal(u_undefined());
    })};
    (null) => {(|b: &mut OpBuilder| {
        b.literal(u_null());
    })};
    ($lit:literal) => {(|b: &mut OpBuilder| {
        prim_to_val!($lit)(b);
    })};
    ($lit:literal) => {(|b: &mut OpBuilder| {
        prim_to_val!($lit)(b);
    })};
    (o:{
        $([$index:expr]: $value:expr)*
    }) => {(|b: &mut OpBuilder| {
        let temp = b.var_t();
        b.var_w_t(temp.clone(), |b| {
            b.obj_e();
        });
        $(
            b.assign_ref(|b| {
                b.var_r_t(temp.clone());
            }, |b| {
                prim_to_val!($index(b));
            }, |b| {
                prim_to_val!($value(b));
            });
        )*
        b.var_r_t(temp);
    })};
    (a: [$($el:expr),*]) => {
        (|b: &mut OpBuilder| {
            let temp = b.var_t();
            b.var_w_t(temp.clone(), |b| {b.arr_e();});
            let mut counter = 0.0;
            $(
                b.assign_ref(|b| {
                    b.var_r_t(temp.clone());
                }, |b| {
                    js_number!(counter);
                }, |b| {
                    $el(b);
                });
            )*
            b.var_r_t(temp);
        })
    };
}

#[macro_export]
macro_rules! js_var {
    ($left:literal = $right:expr) => {
        (|b: &mut OpBuilder| {
            b.var_w(Rc::new($left.to_string()), |b| {
                $right(b);
            });
        })
    };
    ($left:ident = $right:expr) => {(|b: &mut OpBuilder| {
        b.var_w(Rc::new(stringify!($left).to_string()), |b| {
            $right(b);
        });
    })};
    (($left:expr)$([$index:expr])+[$other:expr] = $right:expr) => {(|b: &mut OpBuilder| {
        b.assign_ref(|b| {
            js_index!($left $([$index])+)
        }, |b| {$other(b);}, |b| {$right(b);});
    })};
    (($left:expr)[$other:expr] = $right:expr) => {(|b: &mut OpBuilder| {
        b.assign_ref(|b| {
            $left(b);
        }, |b| {
            $other(b);
        }, |b| {
            $right(b);
        });
    })}
}

#[macro_export]
macro_rules! js_if {
        [($cond:expr) {$($st:expr)*}] => {
            (|b: &mut OpBuilder| {b.ifb(|b| {
                $cond(b);
            }, |b| {
                $($st(b);)*
            });})
        };
}

#[macro_export]
macro_rules! js_if_else {
    (($cond:expr) {$($ist:expr)*} else {$($est:expr)*}) => {
            (|b: &mut OpBuilder| {b.if_elseb(|b| {
                $cond(b);
            }, |b| {
                $($ist(b);)*
            }, |b| {
                $($est(b);)*
            });})
    };
}

#[macro_export]
macro_rules! js_this {
    () => {
        (|b: &mut OpBuilder| {
            let temp = b.this();
            b.var_r_t(temp);
        })
    };
}

#[macro_export]
macro_rules! js_args {
    () => {
        (|b: &mut OpBuilder| {
            b.var_r_t(b.args());
        })
    };
}

// TODO should "this" be a Fn too?
#[macro_export]
macro_rules! js_call {
    (($what:expr)(this: $this:ident, args: $args:expr)) => {
        (|b: &mut OpBuilder| {
            b.call($this.clone(), |b| {
                $args(b);
            });
        })
    };
}

#[macro_export]
macro_rules! js_undefined {
    () => {
        (|b: &mut OpBuilder| {
            b.literal(u_undefined());
        })
    };
}

#[macro_export]
macro_rules! js_number {
    ($n:literal) => {
        (|b: &mut OpBuilder| {
            b.literal(u_number($n as f64));
        })
    };
    ($n:ident) => {
        (|b: &mut OpBuilder| {
            b.literal(u_number($n as f64));
        })
    };
}

#[macro_export]
macro_rules! js_index {
    (($of:expr) $([$index:expr])+[$other:expr]) => {
        {b.deref(|b| {js_index($of$([$index(b)])+)}, |b| {$other(b)});}
    };
    (($of:expr)[$index:expr]) => {
        {{b.deref(|b| {$of(b)}, |b| {$index(b)});}}
    }
}

#[macro_export]
macro_rules! js_native {
    (function ($this:ident, $args:ident) {$($content:expr;)*}) => {
        (|b: &mut OpBuilder| {
                use crate::js::data::js_execution::native_from;
                JsObjectBuilder::new(None)
                    .with_callable(JSCallable::Native {
                        op: native_from(|$this, $args| {

                            $($content;)*

                            #[allow(unreachable_code)]
                            return Ok(u_undefined());
                        }),
                    })
                    .build();
            })
    };
}

#[macro_export]
macro_rules! js_function {
    {$($body:expr)*} => {
        (|b: &mut OpBuilder| {
            b.func(|b| {
                $($body(b);)*
            });
        })
    };
}

fn temp() {
    //OpBuilder::start().func()
    trait ToJs {
        fn to_js(&self) -> JsValue;
    }
    impl ToJs for String {
        fn to_js(&self) -> JsValue {
            u_string(self)
        }
    }
    impl ToJs for &str {
        fn to_js(&self) -> JsValue {
            u_string(self)
        }
    }
    impl ToJs for f64 {
        fn to_js(&self) -> JsValue {
            u_number(*self)
        }
    }
}

pub enum VType {
    Const,
    Let,
    Var,
}
