rclosR :: Rel a -> Rel a
rclosR r = unionSet r (idR background)
