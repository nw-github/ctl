use std::num::NonZeroU32;

use lasso::Rodeo;

pub type StrId = lasso::Spur;

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

#[derive(Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct Strings {
    rodeo: Rodeo,
}

impl Strings {
    pub(crate) const EMPTY: StrId = str_id(SID::EMPTY);
    pub(crate) const UNDERSCORE: StrId = str_id(SID::UNDERSCORE);
    pub(crate) const THIS_PARAM: StrId = str_id(SID::THIS_PARAM);
    pub(crate) const THIS_TYPE: StrId = str_id(SID::THIS_TYPE);
    pub(crate) const TUPLE_ZERO: StrId = str_id(SID::TUPLE_ZERO);
    pub(crate) const TUPLE_NAME: StrId = str_id(SID::TUPLE_NAME);
    pub(crate) const ITER_VAR_NAME: StrId = str_id(SID::ITER_VAR_NAME);
    pub(crate) const SKIP_REASON: StrId = str_id(SID::SKIP_REASON);

    pub(crate) const NULL: StrId = str_id(SID::NULL);
    pub(crate) const SOME: StrId = str_id(SID::SOME);

    pub(crate) const FN_MAIN: StrId = str_id(SID::MAIN);
    pub(crate) const FN_NEW: StrId = str_id(SID::FN_NEW);
    pub(crate) const FN_WRITTEN: StrId = str_id(SID::FN_WRITTEN);
    pub(crate) const FN_INSERT: StrId = str_id(SID::FN_INSERT);
    pub(crate) const FN_WITH_CAPACITY: StrId = str_id(SID::FN_WITH_CAPACITY);

    pub(crate) const VAR_LESS: StrId = str_id(SID::VAR_LESS);
    pub(crate) const VAR_GREATER: StrId = str_id(SID::VAR_GREATER);
    pub(crate) const VAR_EQUAL: StrId = str_id(SID::VAR_EQUAL);

    pub const FEAT_ALLOC: StrId = str_id(SID::FEAT_ALLOC);
    pub const FEAT_IO: StrId = str_id(SID::FEAT_IO);
    pub const FEAT_HOSTED: StrId = str_id(SID::FEAT_HOSTED);
    pub const FEAT_BOEHM: StrId = str_id(SID::FEAT_BOEHM);
    pub const FEAT_TEST: StrId = str_id(SID::FEAT_TEST);
    pub const FEAT_BACKTRACE: StrId = str_id(SID::FEAT_BACKTRACE);

    pub(crate) const ATTR_LANG: StrId = str_id(SID::ATTR_LANG);
    pub(crate) const ATTR_INTRINSIC: StrId = str_id(SID::ATTR_INTRINSIC);
    pub(crate) const ATTR_SAFE: StrId = str_id(SID::ATTR_SAFE);
    pub(crate) const ATTR_MACRO: StrId = str_id(SID::ATTR_MACRO);
    pub(crate) const ATTR_LINKNAME: StrId = str_id(SID::ATTR_LINKNAME);
    pub(crate) const ATTR_PANIC_HANDLER: StrId = str_id(SID::ATTR_PANIC_HANDLER);
    pub(crate) const ATTR_INLINE: StrId = str_id(SID::ATTR_INLINE);
    pub(crate) const ATTR_FEATURE: StrId = str_id(SID::ATTR_FEATURE);
    pub(crate) const ATTR_NOT: StrId = str_id(SID::ATTR_NOT);
    pub(crate) const ATTR_THREAD_LOCAL: StrId = str_id(SID::ATTR_THREAD_LOCAL);
    pub(crate) const ATTR_COLD: StrId = str_id(SID::ATTR_COLD);
    pub(crate) const ATTR_LAYOUT: StrId = str_id(SID::ATTR_LAYOUT);
    pub(crate) const ATTR_ALIGN: StrId = str_id(SID::ATTR_ALIGN);
    pub(crate) const ATTR_TEST_RUNNER: StrId = str_id(SID::ATTR_TEST_RUNNER);
    pub(crate) const ATTR_SKIP: StrId = str_id(SID::SKIP);

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
        assert_eq!(Self::FEAT_ALLOC, rodeo.get_or_intern_static("alloc"));
        assert_eq!(Self::FEAT_IO, rodeo.get_or_intern_static("io"));
        assert_eq!(Self::FEAT_HOSTED, rodeo.get_or_intern_static("hosted"));
        assert_eq!(Self::FEAT_BOEHM, rodeo.get_or_intern_static("boehm"));
        assert_eq!(Self::ITER_VAR_NAME, rodeo.get_or_intern_static("$iter"));
        assert_eq!(Self::FEAT_TEST, rodeo.get_or_intern_static("test"));
        assert_eq!(Self::ATTR_TEST_RUNNER, rodeo.get_or_intern_static("test_runner"));
        assert_eq!(Self::ATTR_SKIP, rodeo.get_or_intern_static("skip"));
        assert_eq!(Self::SKIP_REASON, rodeo.get_or_intern_static("skip_reason"));
        assert_eq!(Self::ATTR_THREAD_LOCAL, rodeo.get_or_intern_static("thread_local"));
        assert_eq!(Self::ATTR_COLD, rodeo.get_or_intern_static("cold"));
        assert_eq!(Self::TUPLE_NAME, rodeo.get_or_intern_static("$tuple"));
        assert_eq!(Self::FEAT_BACKTRACE, rodeo.get_or_intern_static("backtrace"));
        assert_eq!(Self::ATTR_ALIGN, rodeo.get_or_intern_static("align"));
        assert_eq!(Self::ATTR_LAYOUT, rodeo.get_or_intern_static("layout"));
        Self { rodeo }
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
    FEAT_ALLOC,
    FEAT_IO,
    FEAT_HOSTED,
    FEAT_BOEHM,
    ITER_VAR_NAME,
    FEAT_TEST,
    ATTR_TEST_RUNNER,
    SKIP,
    SKIP_REASON,
    ATTR_THREAD_LOCAL,
    ATTR_COLD,
    TUPLE_NAME,
    FEAT_BACKTRACE,
    ATTR_ALIGN,
    ATTR_LAYOUT,
}
