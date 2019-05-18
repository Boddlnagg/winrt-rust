use climeta::schema::TypeDef;

use crate::Result;

pub enum TyDef<'db> {
    Enum(TypeDef<'db>),
    Dummy
}

impl<'db> TyDef<'db> {
    pub fn prepare_enum(ty: TypeDef<'db>) -> Result<TyDef<'db>> {
        //println!("Type: {:?}", ty);
        Ok(TyDef::Enum(ty))
    }

    pub fn dummy() -> Result<TyDef<'db>> {
        Ok(TyDef::Dummy)
    }
    
    pub fn can_be_skipped(&self) -> bool {
        false
    }
}