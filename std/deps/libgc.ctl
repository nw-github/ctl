pub extern fn GC_init();
pub extern fn GC_deinit();

$[unsafe(malloc)]
pub extern fn GC_malloc(size: uint): ?^mut void;
$[unsafe(malloc)]
pub extern fn GC_malloc_atomic(size: uint): ?^mut void;
$[unsafe(malloc)]
pub extern fn GC_malloc_uncollectable(size: uint): ?^mut void;
pub extern fn GC_posix_memalign(ptr: *mut ^mut void, size: uint, align: uint): c_int;
pub extern fn GC_free(ptr: ^mut void);
pub extern fn GC_realloc(ptr: ^mut void, size: uint): ?^mut void;

pub extern fn GC_enable_incremental();

pub extern fn GC_get_parallel(): c_int; /* bool */
pub extern fn GC_set_markers_count(count: c_uint);

pub extern fn GC_get_version(): c_uint;
