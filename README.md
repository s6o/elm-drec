# elm-drec

Elm `Dict` based record with field name and type validation and automatic
decoding from and to JSON.

The design aim is not to be a dictionary for storing any value (there already
are solutions for that), but to provide a better alternative to Elm's standard
record when working with something like PostgREST <https://github.com/begriffs/postgrest>.

## Features / Trade-offs
  * schema based runtime field name and type validation
  * automatic schema based decoding and encoding from and to JSON
  * extensible records (new fields can be added if data is present)
  * No reliance on `Debug.crash`, `Result x a` is used instead

### Supported value types
  * Bool
  * DRec (nesting / sub-records)
  * Float
  * Int
  * Json.Encode.Value
  * Maybe (Bool, DRec, Float, Int, Json.Encode.Value, String)
  * String

## TODO:
  * support for container types: `Array a`, `Dict k v`, `List a`, `Set a`
