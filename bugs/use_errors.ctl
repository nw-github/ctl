mod inner { struct A {} }

use inner::A; // two errors are produced for some reason
