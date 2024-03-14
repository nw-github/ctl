fn assert(cond: bool, msg: str) {
    if !cond {
        panic(msg);
    }
}

fn main() {
    println("\nhash: ");
    {
        println("running map()...");
        hash::map();

        println("running set()...");
        hash::set();

        println("running map_builtin()...");
        hash::map_builtin();
    }

    println("\ntrait default: ");
    {
        println("nooverride_direct()...");
        traits::default::nooverride_direct();

        println("nooverride_dyn()...");
        traits::default::nooverride_dyn();

        println("nooverride_generic()...");
        traits::default::nooverride_generic();

        println("override_direct()...");
        traits::default::override_direct();

        println("override_dyn()...");
        traits::default::override_dyn();

        println("override_generic()...");
        traits::default::override_generic();
    }

    println("\ntrait default by extension: ");
    {
        println("nooverride_direct()...");
        traits::default_ext::nooverride_direct();

        println("nooverride_dyn()...");
        traits::default_ext::nooverride_dyn();

        println("nooverride_generic()...");
        traits::default_ext::nooverride_generic();

        println("override_direct()...");
        traits::default_ext::override_direct();

        println("override_dyn()...");
        traits::default_ext::override_dyn();

        println("override_generic()...");
        traits::default_ext::override_generic();
    }

    println("\nall tests passed!");
}
