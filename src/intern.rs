use std::num::NonZeroU32;

use lasso::Rodeo;

pub type StrId = lasso::Spur;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ByteStrId(NonZeroU32, NonZeroU32);

pub const THIS_PARAM: &str = "this";
pub const THIS_TYPE: &str = "This";

#[derive(Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct Strings {
    #[deref]
    #[deref_mut]
    rodeo: Rodeo,
    byte_str_data: Vec<u8>,
}

impl Strings {
    pub const EMPTY: StrId = str_id(SID::EMPTY);
    pub const UNDERSCORE: StrId = str_id(SID::UNDERSCORE);
    pub const THIS_PARAM: StrId = str_id(SID::THIS_PARAM);
    pub const THIS_TYPE: StrId = str_id(SID::THIS_TYPE);
    pub const TUPLE_ZERO: StrId = str_id(SID::TUPLE_ZERO);
    pub const TUPLE_NAME: StrId = str_id(SID::TUPLE_NAME);
    pub const CLOSURE_NAME: StrId = str_id(SID::CLOSURE_NAME);
    pub const EXTENSION_NAME: StrId = str_id(SID::EXTENSION_NAME);
    pub const FN_TR_ARGS_NAME: StrId = str_id(SID::FN_TR_ARGS_NAME);
    pub const ITER_VAR_NAME: StrId = str_id(SID::ITER_VAR_NAME);
    pub const SKIP_REASON: StrId = str_id(SID::SKIP_REASON);
    pub const UNSAFE: StrId = str_id(SID::UNSAFE);

    pub const NULL: StrId = str_id(SID::NULL);
    pub const SOME: StrId = str_id(SID::SOME);

    pub const FN_MAIN: StrId = str_id(SID::MAIN);
    pub const FN_NEW: StrId = str_id(SID::FN_NEW);
    pub const FN_WRITTEN: StrId = str_id(SID::FN_WRITTEN);
    pub const FN_INSERT: StrId = str_id(SID::FN_INSERT);
    pub const FN_WITH_CAPACITY: StrId = str_id(SID::FN_WITH_CAPACITY);
    pub const FN_CLOSURE_DO_INVOKE: StrId = str_id(SID::FN_CLOSURE_DO_INVOKE);

    pub const VAR_LESS: StrId = str_id(SID::VAR_LESS);
    pub const VAR_GREATER: StrId = str_id(SID::VAR_GREATER);
    pub const VAR_EQUAL: StrId = str_id(SID::VAR_EQUAL);

    pub const ATTR_LANG: StrId = str_id(SID::ATTR_LANG);
    pub const ATTR_INTRINSIC: StrId = str_id(SID::ATTR_INTRINSIC);
    pub const ATTR_SAFE: StrId = str_id(SID::ATTR_SAFE);
    pub const ATTR_MACRO: StrId = str_id(SID::ATTR_MACRO);
    pub const ATTR_LINKNAME: StrId = str_id(SID::ATTR_LINKNAME);
    pub const ATTR_PANIC_HANDLER: StrId = str_id(SID::ATTR_PANIC_HANDLER);
    pub const ATTR_INLINE: StrId = str_id(SID::ATTR_INLINE);
    pub const ATTR_FEATURE: StrId = str_id(SID::ATTR_FEATURE);
    pub const ATTR_CFG: StrId = str_id(SID::ATTR_CFG);
    pub const ATTR_NOT: StrId = str_id(SID::ATTR_NOT);
    pub const ATTR_THREAD_LOCAL: StrId = str_id(SID::ATTR_THREAD_LOCAL);
    pub const ATTR_COLD: StrId = str_id(SID::ATTR_COLD);
    pub const ATTR_LAYOUT: StrId = str_id(SID::ATTR_LAYOUT);
    pub const ATTR_ALIGN: StrId = str_id(SID::ATTR_ALIGN);
    pub const ATTR_TEST_RUNNER: StrId = str_id(SID::ATTR_TEST_RUNNER);
    pub const ATTR_SKIP: StrId = str_id(SID::SKIP);
    pub const ATTR_EXPORT: StrId = str_id(SID::ATTR_EXPORT);
    pub const ATTR_MALLOC: StrId = str_id(SID::ATTR_MALLOC);
    pub const ATTR_NO_MANGLE: StrId = str_id(SID::ATTR_NO_MANGLE);

    pub fn new() -> Self {
        let mut rodeo = Rodeo::default();
        assert_eq!(Self::EMPTY, rodeo.get_or_intern_static(""));
        assert_eq!(Self::THIS_PARAM, rodeo.get_or_intern_static(THIS_PARAM));
        assert_eq!(Self::THIS_TYPE, rodeo.get_or_intern_static(THIS_TYPE));
        assert_eq!(Self::NULL, rodeo.get_or_intern_static("null"));
        assert_eq!(Self::FN_NEW, rodeo.get_or_intern_static("new"));
        assert_eq!(Self::ATTR_LANG, rodeo.get_or_intern_static("lang"));
        assert_eq!(Self::ATTR_INTRINSIC, rodeo.get_or_intern_static("intrinsic"));
        assert_eq!(Self::ATTR_SAFE, rodeo.get_or_intern_static("safe"));
        assert_eq!(Self::FN_MAIN, rodeo.get_or_intern_static("main"));
        assert_eq!(Self::UNDERSCORE, rodeo.get_or_intern_static("_"));
        assert_eq!(Self::SOME, rodeo.get_or_intern_static("Some"));
        assert_eq!(Self::TUPLE_ZERO, rodeo.get_or_intern_static("0"));
        assert_eq!(Self::ATTR_MACRO, rodeo.get_or_intern_static("c_macro"));
        assert_eq!(Self::ATTR_LINKNAME, rodeo.get_or_intern_static("link_name"));
        assert_eq!(Self::FN_WRITTEN, rodeo.get_or_intern_static("written"));
        assert_eq!(Self::FN_INSERT, rodeo.get_or_intern_static("insert"));
        assert_eq!(Self::VAR_LESS, rodeo.get_or_intern_static("Less"));
        assert_eq!(Self::VAR_GREATER, rodeo.get_or_intern_static("Greater"));
        assert_eq!(Self::VAR_EQUAL, rodeo.get_or_intern_static("Equal"));
        assert_eq!(Self::ATTR_PANIC_HANDLER, rodeo.get_or_intern_static("panic_handler"));
        assert_eq!(Self::ATTR_INLINE, rodeo.get_or_intern_static("inline"));
        assert_eq!(Self::FN_WITH_CAPACITY, rodeo.get_or_intern_static("with_capacity"));
        assert_eq!(Self::ATTR_FEATURE, rodeo.get_or_intern_static("feature"));
        assert_eq!(Self::ATTR_NOT, rodeo.get_or_intern_static("not"));
        assert_eq!(Self::ITER_VAR_NAME, rodeo.get_or_intern_static("$iter"));
        assert_eq!(Self::ATTR_TEST_RUNNER, rodeo.get_or_intern_static("test_runner"));
        assert_eq!(Self::ATTR_SKIP, rodeo.get_or_intern_static("skip"));
        assert_eq!(Self::SKIP_REASON, rodeo.get_or_intern_static("skip_reason"));
        assert_eq!(Self::ATTR_THREAD_LOCAL, rodeo.get_or_intern_static("thread_local"));
        assert_eq!(Self::ATTR_COLD, rodeo.get_or_intern_static("cold"));
        assert_eq!(Self::TUPLE_NAME, rodeo.get_or_intern_static("$tuple"));
        assert_eq!(Self::CLOSURE_NAME, rodeo.get_or_intern_static("{closure}"));
        assert_eq!(Self::EXTENSION_NAME, rodeo.get_or_intern_static("{extension}"));
        assert_eq!(Self::ATTR_ALIGN, rodeo.get_or_intern_static("align"));
        assert_eq!(Self::ATTR_LAYOUT, rodeo.get_or_intern_static("layout"));
        assert_eq!(Self::FN_TR_ARGS_NAME, rodeo.get_or_intern_static("args"));
        assert_eq!(Self::FN_CLOSURE_DO_INVOKE, rodeo.get_or_intern_static("do_invoke"));
        assert_eq!(Self::ATTR_EXPORT, rodeo.get_or_intern_static("export"));
        assert_eq!(Self::ATTR_CFG, rodeo.get_or_intern_static("cfg"));
        assert_eq!(Self::UNSAFE, rodeo.get_or_intern_static("unsafe"));
        assert_eq!(Self::ATTR_MALLOC, rodeo.get_or_intern_static("malloc"));
        assert_eq!(Self::ATTR_NO_MANGLE, rodeo.get_or_intern_static("no_mangle"));
        Self { rodeo, byte_str_data: vec![0] }
    }

    pub fn intern_byte_str(&mut self, str: impl AsRef<[u8]>) -> ByteStrId {
        let str = str.as_ref();
        let (begin, end) = unsafe {
            (
                NonZeroU32::new_unchecked(self.byte_str_data.len() as u32),
                NonZeroU32::new_unchecked((self.byte_str_data.len() + str.len()) as u32),
            )
        };

        self.byte_str_data.extend_from_slice(str);
        ByteStrId(begin, end)
    }

    pub fn resolve_byte_str(&self, id: ByteStrId) -> &[u8] {
        &self.byte_str_data[id.0.get() as usize..id.1.get() as usize]
    }
}

impl Default for Strings {
    fn default() -> Self {
        Self::new()
    }
}

const fn str_id(value: SID) -> StrId {
    unsafe { std::mem::transmute(NonZeroU32::new_unchecked(value as u32)) }
}

#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
enum SID {
    EMPTY = 1,
    THIS_PARAM,
    THIS_TYPE,
    NULL,
    FN_NEW,
    ATTR_LANG,
    ATTR_INTRINSIC,
    ATTR_SAFE,
    MAIN,
    UNDERSCORE,
    SOME,
    TUPLE_ZERO,
    ATTR_MACRO,
    ATTR_LINKNAME,
    FN_WRITTEN,
    FN_INSERT,
    VAR_LESS,
    VAR_GREATER,
    VAR_EQUAL,
    ATTR_PANIC_HANDLER,
    ATTR_INLINE,
    FN_WITH_CAPACITY,
    ATTR_FEATURE,
    ATTR_NOT,
    ITER_VAR_NAME,
    ATTR_TEST_RUNNER,
    SKIP,
    SKIP_REASON,
    ATTR_THREAD_LOCAL,
    ATTR_COLD,
    TUPLE_NAME,
    CLOSURE_NAME,
    EXTENSION_NAME,
    ATTR_ALIGN,
    ATTR_LAYOUT,
    FN_TR_ARGS_NAME,
    FN_CLOSURE_DO_INVOKE,
    ATTR_EXPORT,
    ATTR_CFG,
    UNSAFE,
    ATTR_MALLOC,
    ATTR_NO_MANGLE,
}
