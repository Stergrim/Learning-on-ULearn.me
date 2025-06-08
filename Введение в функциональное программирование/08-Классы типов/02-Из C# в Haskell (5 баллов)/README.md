# Из C# в Haskell

Перепишите этот код, использующий интерфейсы и абстрактные классы в C#, на Haskell с помощью классов типов.

В качестве имён типов и классов используйте имена из C#. Если нужно, замените первую букву в названии на маленькую.

```hs
public interface SearchQuery {
    String[] ToTerms();
}

public record GeoArea
{
    public double Longitude { get; init; }
    public double Latitude { get; init; }
    public double Radius { get; init; }
}

public abstract class GeoSearchQuery : SearchQuery {
    public abstract String[] ToTerms();

    public double ComputeApproxTotalArea() {
        return ToGeoAreas().Select(ComputeApproxArea).Sum();
    },

    private static double ComputeApproxArea(GeoArea geoArea){
        return System.Math.PI * geoArea.Radius * geoArea.Radius
    }

    public abstract GeoArea[] ToGeoAreas();
}

public interface AuthenticatedSearchQuery : SearchQuery {
    String GetIdentity();
}
```


Все тесты пройдены, задача сдана:
```hs
{-# LANGUAGE DefaultSignatures #-}

-- Interface equivalent for SearchQuery
class SearchQuery a where
    toTerms :: a -> [String]
    toTerms _ = []

-- GeoArea record
data GeoArea = GeoArea
    { longitude :: Double
    , latitude  :: Double
    , radius    :: Double
    } deriving (Show)

-- Type class for GeoSearchQuery
class SearchQuery a => GeoSearchQuery a where
    toGeoAreas :: a -> [GeoArea]
    
    computeApproxTotalArea :: a -> Double
    computeApproxTotalArea a = sum $ map computeApproxArea (toGeoAreas a)
        where computeApproxArea geo = pi * radius geo * radius geo

-- Type class for AuthenticatedSearchQuery
class SearchQuery a => AuthenticatedSearchQuery a where
    getIdentity :: a -> String
```
