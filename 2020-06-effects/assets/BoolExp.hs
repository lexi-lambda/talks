parseAsSTDWithinObj obj = do
  distanceVal <- onNothing (OMap.lookup "distance" obj) $
            throw500 "expected \"distance\" input field in st_d_within"
  dist <- mkParameterizablePGValue <$> asPGColumnValue distanceVal
  fromVal <- onNothing (OMap.lookup "from" obj) $
            throw500 "expected \"from\" input field in st_d_within"
  from <- mkParameterizablePGValue <$> asPGColumnValue fromVal
    PGColumnScalar PGGeography -> do
        onNothing (OMap.lookup "use_spheroid" obj) $
        throw500 "expected \"use_spheroid\" input field in st_d_within"
      return $ ASTDWithinGeog $ DWithinGeogOp dist from useSpheroid
    PGColumnScalar PGGeometry ->
      return $ ASTDWithinGeom $ DWithinGeomOp dist from
    _ -> throw500 "expected PGGeometry/PGGeography column for st_d_within"
parseAsSTIntersectsNbandGeomObj obj = do
  nbandVal <- onNothing (OMap.lookup "nband" obj) $
              throw500 "expected \"nband\" input field"
  nband <- mkParameterizablePGValue <$> asPGColumnValue nbandVal
  geommin <- parseGeommin obj
  return $ ASTIntersectsNbandGeom $ STIntersectsNbandGeommin nband geommin
parseAsSTIntersectsGeomNbandObj obj = do
  nbandMM <- fmap (fmap mkParameterizablePGValue) <$>
    traverse asPGColumnValueM (OMap.lookup "nband" obj)
  geommin <- parseGeommin obj
parseGeommin obj = do
  geomminVal <- onNothing (OMap.lookup "geommin" obj) $
                throw500 "expected \"geommin\" input field"
  mkParameterizablePGValue <$> asPGColumnValue geomminVal
