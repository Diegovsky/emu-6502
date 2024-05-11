macro_rules! multivalue {
    ($item:item) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        $item
    };
}

pub(crate) use multivalue as multivalue;
