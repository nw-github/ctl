fn assert(cond: bool, msg: str) {
    if !cond {
        panic(msg);
    }
}

fn main() {
    std::println("span: ");
    {
        std::println("running ranges()...");
        span::ranges();

        std::println("pattern()...");
        span::pattern();

        std::println("pattern_destructure()...");
        span::pattern_destructure();

        std::println("pattern_destructure_2()...");
        span::pattern_destructure_2();
    }

    std::println("\nvec: ");
    {
        std::println("running init1()...");
        vec::init1();

        std::println("running init2()...");
        vec::init2();
        
        std::println("running push_pop()...");
        vec::push_pop();

        std::println("running insert()...");
        vec::insert();

        std::println("running remove()...");
        vec::remove();

        std::println("running swap_remove()...");
        vec::swap_remove();
    }

    std::println("\nhash: ");
    {
        std::println("running map()...");
        hash::map();

        std::println("running set()...");
        hash::set();
    }

    std::println("\norder of evaluation: ");
    {
        std::println("blocks(trigger_else: false)...");
        ordering::blocks(false);

        std::println("blocks(trigger_else: true)...");
        ordering::blocks(true);

        std::println("positional_call()...");
        ordering::positional_call();

        std::println("increment()...");
        ordering::increment();

        std::println("instance_ordering()...");
        ordering::instance_ordering();

        std::println("keyword_call()...");
        ordering::keyword_call();
    }

    std::println("\narray: ");
    {
        std::println("pattern_stuff()...");
        array::pattern_stuff();
    
        std::println("nested_ptr()...");
        array::nested_ptr();

        std::println("nested_destructure_1()...");
        array::nested_destructure_1();

        std::println("nested_destructure_2()...");
        array::nested_destructure_2();

        std::println("nested_destructure_3()...");
        array::nested_destructure_3();

        std::println("nested_destructure_4()...");
        array::nested_destructure_4();

        std::println("nested_destructure_5()...");
        array::nested_destructure_5();
    }

    std::println("\npatterns: ");
    {
        std::println("struct_pattern()...");
        patterns::struct_pattern();

        std::println("union_pattern()...");
        patterns::union_pattern();

        std::println("union_struct_pattern()...");
        patterns::union_struct_pattern();

        std::println("option_struct_pattern()...");
        patterns::option_struct_pattern();

        std::println("integer_pattern()...");
        patterns::integer_pattern();

        std::println("string_pattern()...");
        patterns::string_pattern();
    }

    std::println("\nmisc: ");
    {
        std::println("booleans()...");
        misc::booleans();

        std::println("positional::start()...");
        positional::start();

        std::println("positional::middle()...");
        positional::middle();

        std::println("positional::end()...");
        positional::end();

        std::println("struct_order()...");
        struct_order::test();

        std::println("modules()...");
        modules::test();

        std::println("statics()...");
        misc::statics();

        std::println("void_assigns()...");
        misc::void_assigns();

        std::println("sizes()...");
        misc::sizes();
    }

    std::println("\nloops: ");
    {
        std::println("while_loop()...");
        loops::while_loop();

        std::println("for_loop()...");
        loops::for_loop();

        std::println("infinite_loop()...");
        loops::infinite_loop();
    }

    std::println("\ndynamic dispatch: ");
    {
        std::println("normal()...");
        traits::dd::normal();

        std::println("dependant()...");
        traits::dd::dependant();

        std::println("recursive()...");
        traits::dd::recursive();
    }

    std::println("\ntrait default: ");
    {
        std::println("nooverride_direct()...");
        traits::default::nooverride_direct();

        std::println("nooverride_dyn()...");
        traits::default::nooverride_dyn();

        std::println("nooverride_generic()...");
        traits::default::nooverride_generic();

        std::println("override_direct()...");
        traits::default::override_direct();

        std::println("override_dyn()...");
        traits::default::override_dyn();

        std::println("override_generic()...");
        traits::default::override_generic();
    }

    std::println("\ntrait default by extension: ");
    {
        std::println("nooverride_direct()...");
        traits::default_ext::nooverride_direct();

        std::println("nooverride_dyn()...");
        traits::default_ext::nooverride_dyn();

        std::println("nooverride_generic()...");
        traits::default_ext::nooverride_generic();

        std::println("override_direct()...");
        traits::default_ext::override_direct();

        std::println("override_dyn()...");
        traits::default_ext::override_dyn();

        std::println("override_generic()...");
        traits::default_ext::override_generic();
    }

    std::println("\nall tests passed!");
}
