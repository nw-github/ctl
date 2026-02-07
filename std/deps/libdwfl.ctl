use super::libc::posix::pid_t;

pub union Dwfl_Module {}
pub union Dwfl_Line {}
pub union Elf {}
pub union Dwfl {}

pub type Dwarf_Addr = uint;
pub type Dwarf_Word = GElf_XWord;

pub type GElf_Word = u32;
pub type GElf_XWord = u64;
pub type GElf_Addr = uint;
pub type GElf_Off = uint;
pub type GElf_Sym = Elf64_Sym;

$[layout(C)]
pub unsafe union GElf_Shdr { _pad: [u8; 64], _align: u64 }

$[layout(C)]
pub unsafe union Elf64_Sym { _pad: [u8; 24], _align: u64 }

$[layout(C)]
pub struct Dwfl_Callbacks {
    pub find_elf: ?extern fn(
        module:   ?*mut Dwfl_Module,
        userdata: ?^mut ?^mut void,
        modname:  ?^c_char,
        base:     Dwarf_Addr,
        filename: ?^mut ^mut c_char,
        elfp:     ?^mut ^mut Elf,
    ) => c_int,
    pub find_debuginfo: ?extern fn(
        module:   ?*mut Dwfl_Module,
        userdata: ?^mut ?^mut void,
        modname:  ?^c_char,
        base:     Dwarf_Addr,
        filename: ?^c_char,
        debuglink_file: ?^c_char,
        debuglink_crc: GElf_Word,
        debuginfo_file_name: ?^mut ?^mut c_char,
    ) => c_int,
    pub section_address: ?extern fn(
        module:   ?*mut Dwfl_Module,
        userdata: ?^mut ?^mut void,
        modname:  ?^c_char,
        base:     Dwarf_Addr,
        secname:  ?^c_char,
        shndx:    GElf_Word,
        shdr:     ?*GElf_Shdr,
        addr:     ?*mut Dwarf_Addr,
    ) => c_int,
    pub debuginfo_path: ?^mut ^mut c_char,
}

pub extern fn dwfl_linux_proc_find_elf(
    module:   ?*mut Dwfl_Module,
    userdata: ?^mut ?^mut void,
    modname:  ?^c_char,
    base:     Dwarf_Addr,
    filename: ?^mut ^mut c_char,
    elfp:     ?^mut ^mut Elf,
): c_int;

pub extern fn dwfl_standard_find_debuginfo(
    module:   ?*mut Dwfl_Module,
    userdata: ?^mut ?^mut void,
    modname:  ?^c_char,
    base:     Dwarf_Addr,
    filename: ?^c_char,
    debuglink_file: ?^c_char,
    debuglink_crc: GElf_Word,
    debuginfo_file_name: ?^mut ?^mut c_char,
): c_int;

pub extern fn dwfl_begin(cbs: *Dwfl_Callbacks): ?*mut Dwfl;
pub extern fn dwfl_end(dwfl: *mut Dwfl);
pub extern fn dwfl_report_begin(dwfl: *mut Dwfl);
pub extern fn dwfl_linux_proc_report(dwfl: *mut Dwfl, pid: pid_t): c_int;
pub extern fn dwfl_report_end(
    dwfl: *mut Dwfl,
    removed: ?extern fn(
        ?*mut Dwfl_Module,
        ?^mut void,
        ^c_char,
        Dwarf_Addr,
        arg: ?^mut void) => c_int,
    arg: ?^mut void,
): c_int;

pub extern fn dwfl_getsrc(dwfl: *mut Dwfl, addr: Dwarf_Addr): ?*mut Dwfl_Line;
pub extern fn dwfl_lineinfo(
    dwfl:   *mut Dwfl_Line,
    addr:   *mut Dwarf_Addr,
    linep:  ?*mut c_int,
    colp:   ?*mut c_int,
    mtime:  ?*mut Dwarf_Word,
    length: ?*mut Dwarf_Word,
): ?^c_char;

pub extern fn dwfl_addrmodule(dwfl: *mut Dwfl, addr: Dwarf_Addr): ?*mut Dwfl_Module;
pub extern fn dwfl_module_addrinfo(
    module: *mut Dwfl_Module,
    address: GElf_Addr,
    offset: *mut GElf_Off,
    sym:    ^mut GElf_Sym,
    shndxp: ?*mut GElf_Word,
    elfp:   ?^mut ^mut Elf,
    bias:   ?*mut Dwarf_Addr,
): ?^c_char;
