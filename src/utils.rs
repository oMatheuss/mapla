pub struct HexSlice<'a>(&'a [u8]);

impl<'a> HexSlice<'a> {
    pub fn new<T>(slice: &'a T) -> HexSlice<'a>
    where
        T: ?Sized + AsRef<[u8]> + 'a,
    {
        HexSlice(slice.as_ref())
    }
}

impl std::fmt::LowerHex for HexSlice<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() > 0 {
            write!(f, "0x{:x}", self.0[0])?;
            for i in 1..self.0.len() {
                write!(f, ",0x{:x}", self.0[i])?;
            }
        }
        Ok(())
    }
}
