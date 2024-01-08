fn assert(cond: bool, msg: str) {
    if !cond {
        core::panic(msg);
    }
}

fn main() {
    std::println("span: ");
    {
        std::println("running ranges()...");
        span::ranges();
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

        std::println("array_pattern()...");
        patterns::array_pattern();

        std::println("array_ptr_pattern()...");
        patterns::array_ptr_pattern();
    
        std::println("nested_ptr_by_val()...");
        patterns::nested_ptr_by_val();
    
        std::println("nested_ptr_by_ref()...");
        patterns::nested_ptr_by_ref();

        std::println("nested_destructure_1()...");
        patterns::nested_destructure_1();

        std::println("nested_destructure_2()...");
        patterns::nested_destructure_2();

        std::println("nested_destructure_3()...");
        patterns::nested_destructure_3();

        std::println("nested_destructure_4()...");
        patterns::nested_destructure_4();
    }

    std::println("\nmisc: ");
    {
        std::println("booleans()...");
        bool::booleans();

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
    }

    std::println("\nall tests passed!");
}
