use crate::js::data::js_types::JsValue;
use crate::js::data::util::s_pool;
use std::path::{Path, PathBuf};
use std::rc::Rc;

pub trait JSResolver {
    /// For Some, tries executing the returned JS Function (must be a function).
    /// For None, tries the next resolver
    fn require(&self, val: Rc<String>) -> Option<JsValue>;
}

pub struct FileResolver {
    base: PathBuf,
}

impl FileResolver {
    pub fn new(base: impl AsRef<Path>) -> FileResolver {
        FileResolver {
            base: std::fs::canonicalize(base)
                .expect("That file-resolver path does not exist or could not be accessed"),
        }
    }
}

impl JSResolver for FileResolver {
    fn require(&self, val: Rc<String>) -> Option<Result<JsValue, JsValue>> {
        if !val.starts_with(".") {
            return None;
        }

        let canon_path = std::fs::canonicalize(self.base.as_os_str() + val.as_str());

        // TODO make sure path is in base / no path traversal above base
        if let Err(_) = canon_path {
            return Some(Err(JsValue::String(s_pool("Path not resolved"))));
        }

        // TODO limit file size to match memory requirements
        let content = std::fs::read(canon_path.unwrap());
        if content.is_err() {
            return Some(Err(JsValue::String(s_pool("Could not read file"))));
        }

        let content: String = content.unwrap().into();

        unimplemented!()
    }
}
