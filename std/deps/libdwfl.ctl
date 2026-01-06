pub union Dwfl_Module {}
pub union Dwfl_Line {}
pub union Elf {}
pub union Dwfl {}

@(layout(C))
pub unsafe union GElf_Shdr { _pad: [u8; 64], _align: u64 }

@(layout(C))
pub unsafe union Elf64_Sym { _pad: [u8; 24], _align: u64 }

@(layout(C))
pub struct Dwfl_Callbacks {
    pub find_elf: ?extern unsafe fn(
        module:   ?*mut Dwfl_Module,
        userdata: ?^mut ?^mut void,
        modname:  ?^c_char,
        base:     uint /* Dwarf_Addr */,
        filename: ?^mut ^mut c_char,
        elfp:     ?^mut ^mut Elf,
    ) => c_int,
    pub find_debuginfo: ?extern unsafe fn(
        module:   ?*mut Dwfl_Module,
        userdata: ?^mut ?^mut void,
        modname:  ?^c_char,
        base:     uint /* Dwarf_Addr */,
        filename: ?^c_char,
        debuglink_file: ?^c_char,
        debuglink_crc: u32 /* GElf_Word */,
        debuginfo_file_name: ?^mut ?^mut c_char,
    ) => c_int,
    pub section_address: ?extern unsafe fn(
        module:   ?*mut Dwfl_Module,
        userdata: ?^mut ?^mut void,
        modname:  ?^c_char,
        base:     uint /* Dwarf_Addr */,
        secname:  ?^c_char,
        shndx:    u32 /* GElf_Word */,
        shdr:     ?*GElf_Shdr,
        addr:     ?*mut uint /* Dwarf_Addr */,
    ) => c_int,
    pub debuginfo_path: ?^mut ^mut c_char,
}

pub extern fn dwfl_linux_proc_find_elf(
    module:   ?*mut Dwfl_Module,
    userdata: ?^mut ?^mut void,
    modname:  ?^c_char,
    base:     uint /* Dwarf_Addr */,
    filename: ?^mut ^mut c_char,
    elfp:     ?^mut ^mut Elf,
): c_int;

pub extern fn dwfl_standard_find_debuginfo(
    module:   ?*mut Dwfl_Module,
    userdata: ?^mut ?^mut void,
    modname:  ?^c_char,
    base:     uint /* Dwarf_Addr */,
    filename: ?^c_char,
    debuglink_file: ?^c_char,
    debuglink_crc: u32 /* GElf_Word */,
    debuginfo_file_name: ?^mut ?^mut c_char,
): c_int;

pub extern fn dwfl_begin(cbs: *Dwfl_Callbacks): ?*mut Dwfl;
pub extern fn dwfl_end(dwfl: *mut Dwfl);
pub extern fn dwfl_report_begin(dwfl: *mut Dwfl);
pub extern fn dwfl_linux_proc_report(dwfl: *mut Dwfl, pid: c_int /* pid_t */): c_int;
pub extern fn dwfl_report_end(
    dwfl: *mut Dwfl,
    removed: ?extern unsafe fn(
        ?*mut Dwfl_Module,
        ?^mut void,
        ^c_char,
        uint /* Dwarf_Addr */,
        arg: ?^mut void) => c_int,
    arg: ?^mut void,
): c_int;

pub extern fn dwfl_getsrc(dwfl: *mut Dwfl, addr: uint /* Dwfl_Addr */): ?*mut Dwfl_Line;
pub extern fn dwfl_lineinfo(
    dwfl:   *mut Dwfl_Line,
    addr:   *mut uint /* Dwarf_Addr */,
    linep:  ?*mut c_int,
    colp:   ?*mut c_int,
    mtime:  ?*mut u64 /* Dwarf_Word */,
    length: ?*mut u64 /* Dwarf_Word */,
): ?^c_char;

pub extern fn dwfl_addrmodule(dwfl: *mut Dwfl, addr: uint /* Dwfl_Addr */): ?*mut Dwfl_Module;
pub extern fn dwfl_module_addrinfo(
    module: *mut Dwfl_Module,
    address: uint /* GElf_Addr */,
    offset: ?*mut uint /* GElf_Off */,
    sym:    *mut Elf64_Sym /* GElf_Sym */,
    shndxp: ?*mut u32 /* GElf_Word */,
    elfp:   ?^mut ^mut Elf,
    bias:   ?*mut uint /* Dwarf_Addr */,
): ?^c_char;
