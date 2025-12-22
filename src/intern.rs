use std::num::NonZeroU32;

use lasso::Rodeo;

pub type StrId = lasso::Spur;

pub(crate) const THIS_PARAM: &str = "this";
pub(crate) const THIS_TYPE: &str = "This";

#[derive(Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct Strings {
    #[deref]
    #[deref_mut]
    rodeo: Rodeo,
}

impl Strings {
    pub const EMPTY: StrId = invent_str_id(1);
    pub const THIS_PARAM: StrId = invent_str_id(2);
    pub const THIS_TYPE: StrId = invent_str_id(3);
    pub const NULL: StrId = invent_str_id(4);
    pub const NEW: StrId = invent_str_id(5);
    pub const ATTR_LANG: StrId = invent_str_id(6);
    pub const ATTR_INSTRINSIC: StrId = invent_str_id(7);
    pub const ATTR_SAFE: StrId = invent_str_id(8);
    pub const LANG_SPAN: StrId = invent_str_id(9);
    pub const LANG_SPAN_MUT: StrId = invent_str_id(10);
    pub const MAIN: StrId = invent_str_id(11);
    pub const UNDERSCORE: StrId = invent_str_id(12);
    pub const SOME: StrId = invent_str_id(13);
    pub const TUPLE_ZERO: StrId = invent_str_id(14);
    pub const LANG_STRING: StrId = invent_str_id(15);
    pub const LANG_OPTION: StrId = invent_str_id(16);
    pub const LANG_NUMERIC: StrId = invent_str_id(17);
    pub const LANG_INTEGRAL: StrId = invent_str_id(18);
    pub const LANG_SIGNED: StrId = invent_str_id(19);
    pub const LANG_UNSIGNED: StrId = invent_str_id(20);
    pub const ATTR_NOGEN: StrId = invent_str_id(21);
    pub const ATTR_LINKNAME: StrId = invent_str_id(22);
    pub const LANG_FMT_ARG: StrId = invent_str_id(23);
    pub const FN_WRITTEN: StrId = invent_str_id(24);
    pub const FN_INSERT: StrId = invent_str_id(25);
    pub const LANG_ARRAY: StrId = invent_str_id(26);
    pub const VAR_LESS: StrId = invent_str_id(27);
    pub const VAR_GREATER: StrId = invent_str_id(28);
    pub const VAR_EQUAL: StrId = invent_str_id(29);
    pub const ATTR_PANIC_HANDLER: StrId = invent_str_id(30);
    pub const ATTR_INLINE: StrId = invent_str_id(31);
    pub const FN_WITH_CAPACITY: StrId = invent_str_id(32);
    pub const ATTR_FEATURE: StrId = invent_str_id(33);
    pub const ATTR_NOT: StrId = invent_str_id(34);
    pub const FEAT_ALLOC: StrId = invent_str_id(35);
    pub const FEAT_IO: StrId = invent_str_id(36);
    pub const FEAT_HOSTED: StrId = invent_str_id(37);
    pub const FEAT_BOEHM: StrId = invent_str_id(38);

    pub fn new() -> Self {
        let mut rodeo = Rodeo::default();
        assert_eq!(Self::EMPTY, rodeo.get_or_intern_static(""));
        assert_eq!(Self::THIS_PARAM, rodeo.get_or_intern_static(THIS_PARAM));
        assert_eq!(Self::THIS_TYPE, rodeo.get_or_intern_static(THIS_TYPE));
        assert_eq!(Self::NULL, rodeo.get_or_intern_static("null"));
        assert_eq!(Self::NEW, rodeo.get_or_intern_static("new"));
        assert_eq!(Self::ATTR_LANG, rodeo.get_or_intern_static("lang"));
        assert_eq!(Self::ATTR_INSTRINSIC, rodeo.get_or_intern_static("intrinsic"));
        assert_eq!(Self::ATTR_SAFE, rodeo.get_or_intern_static("safe"));
        assert_eq!(Self::LANG_SPAN, rodeo.get_or_intern_static("span"));
        assert_eq!(Self::LANG_SPAN_MUT, rodeo.get_or_intern_static("span_mut"));
        assert_eq!(Self::MAIN, rodeo.get_or_intern_static("main"));
        assert_eq!(Self::UNDERSCORE, rodeo.get_or_intern_static("_"));
        assert_eq!(Self::SOME, rodeo.get_or_intern_static("Some"));
        assert_eq!(Self::TUPLE_ZERO, rodeo.get_or_intern_static("0"));
        assert_eq!(Self::LANG_STRING, rodeo.get_or_intern_static("string"));
        assert_eq!(Self::LANG_OPTION, rodeo.get_or_intern_static("option"));
        assert_eq!(Self::LANG_NUMERIC, rodeo.get_or_intern_static("numeric"));
        assert_eq!(Self::LANG_INTEGRAL, rodeo.get_or_intern_static("integral"));
        assert_eq!(Self::LANG_SIGNED, rodeo.get_or_intern_static("signed"));
        assert_eq!(Self::LANG_UNSIGNED, rodeo.get_or_intern_static("unsigned"));
        assert_eq!(Self::ATTR_NOGEN, rodeo.get_or_intern_static("c_opaque"));
        assert_eq!(Self::ATTR_LINKNAME, rodeo.get_or_intern_static("c_name"));
        assert_eq!(Self::LANG_FMT_ARG, rodeo.get_or_intern_static("fmt_arg"));
        assert_eq!(Self::FN_WRITTEN, rodeo.get_or_intern_static("written"));
        assert_eq!(Self::FN_INSERT, rodeo.get_or_intern_static("insert"));
        assert_eq!(Self::LANG_ARRAY, rodeo.get_or_intern_static("array"));
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
        Self { rodeo }
    }
}

impl Default for Strings {
    fn default() -> Self {
        Self::new()
    }
}

const fn invent_str_id(value: u32) -> StrId {
    unsafe { std::mem::transmute(NonZeroU32::new_unchecked(value)) }
}
