#[derive(Hash, Debug)]
pub struct Id {
    pub id: usize,
    pub name: String,
    /// Path in the linux format of {roots}/{stem}/{stem}/
    pub origin: String,
}

pub struct IdFactory {
    id_enumerator: usize,
}

impl IdFactory {
    pub fn new() -> Self {
        Self { id_enumerator: 0 }
    }

    pub fn id(&mut self, name: String, origin: String) -> Id {
        let id = Id {
            id: self.id_enumerator,
            name,
            origin,
        };
        self.id_enumerator += 1;

        id
    }
}
