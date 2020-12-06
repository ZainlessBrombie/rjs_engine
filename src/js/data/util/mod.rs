use crate::js::data::js_execution::{EngineQueuer, FnOpRepr, JsVar, VarAlloc};
use crate::js::data::js_types::{Identity, JSCallable, JsObj, JsProperty, JsValue, JsFn};
use crate::js::data::EngineConstants::ConstantStrings;
use gc::{Gc, GcCell};
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

    pub fn with_prop(mut self, key: Rc<String>, value: JsValue) -> Self {
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

    pub fn with_proto(mut self, proto: JsValue) -> Self {
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
    static str_pool: RefCell<HashMap<usize, Rc<String>>> = RefCell::new(Default::default());
}

pub fn s_pool(s: &'static str) -> Rc<String> {
    str_pool.with(|map| {
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
        let mut ret = RefCell::borrow_mut(this)
            .cur
            .get(name)
            .map(|r| {
                if heaped {
                    return VarAlloc::CapturedAt {
                        name: name.clone(),
                        from: Box::new(r.clone()),
                        target: 0,
                    };
                }
                r.clone()
            })
            .or_else(|| {
                RefCell::borrow(this).prev.as_ref().map_or(None, |mut p| {
                    if heaped {
                        Sl::get(p, name, false).map(|source| {
                            return VarAlloc::CapturedAt {
                                name: name.clone(),
                                from: Box::new(source),
                                target: 0,
                            };
                        })
                    } else {
                        ScopeLookup::get(&this, &name, RefCell::borrow(this).heap_break)
                        // TODO possibly incorrect
                    }
                })
            });
        if let Some(ret) = &mut ret {
            if let VarAlloc::CapturedAt { target, .. } = ret {
                let this = RefCell::borrow_mut(this);
                this.local_counter += 1;
                *target = this.local_counter - 1;
            }
        }
        return ret;
    }

    pub fn insert_here(this: &Scope, name: Rc<String>) -> VarAlloc {
        return ScopeLookup::get(&this, &name, false).unwrap_or_else(|| {
            let this = RefCell::borrow_mut(this);
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
        if let Some(prev) = &RefCell::borrow(this).prev {
            return ScopeLookup::insert_top(prev, name);
        } else {
            let v = VarAlloc::Static(name, JsVar::new_t());
            RefCell::borrow(this).cur.insert(name.clone(), v.clone());
            return v;
        }
    }

    pub fn temp_var(this: &Scope) -> VarAlloc {
        let mut this = borm(this);
        this.local_counter += 1;
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

    pub fn var_dec(&mut self, t: VType, name: Rc<String>) -> &mut Self {
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
        let result = self.sub_b();
        what(self);
        self.statements.push(FnOpRepr::Assign {
            target: Sl::get_or_global(&self.scopes, name),
            what: Rc::new(FnOpRepr::Multi {
                block: result.statements,
            }),
        });
        self
    }

    pub fn var_w_t(&mut self, v: VarAlloc, what: impl FnOnce(&mut OpBuilder)) -> &mut OpBuilder {
        let result = self.sub_b();
        what(self);
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
        let mut i_b = self.sub_b();
        let mut e_b = self.sub_b();

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
        let cond_r = self.sub_b();
        let body_r = self.sub_b();
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
        let mut init_r = self.sub_b();
        let mut cond_r = self.sub_b();
        let mut each_r = self.sub_b();
        let mut body_r = self.sub_b();

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

    pub fn array_e(&mut self) -> &mut OpBuilder {
        self.statements.push(FnOpRepr::NewObject { is_array: true });
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
                block: k.statements,
            }),
            key: Rc::new(FnOpRepr::Multi {
                block: v.statements,
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
            .with_callable(JSCallable::Js { content: Rc::new("".to_string()), creator: Gc::new(JsFn { 
                ops: Rc::new(FnOpRepr::Multi { block: self.statements }), 
                captures: Rc::new(vec![])
            })})
            .build()
    }
}

pub enum VType {
    Const,
    Let,
    Var,
}
