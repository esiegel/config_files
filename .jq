# Simplifies a schema object if all of the values are the same
# and there are greater than 10 entries in the object.
#
# Transforms:
#   { "a": "NUMBER", ..., "z": "NUMBER" }
# To
#   { "KEY": "NUMBER" }
def simplify_schema_obj:
  def values(obj): obj | to_entries | map(.value);
  def all_values_same(obj): values(obj) | unique | length == 1;

  # Return the transformed object
  if all_values_same(.) and length > 10 then { "KEY": values(.)[0] } else . end;

# Given any JSON type, generates a schema
#
# Example:
#     {
#       "a": "STRING",
#       "b": [
#         "STRING"
#       ],
#       "c": [
#         "NUMBER"
#       ],
#       "gene_index_map": {
#         "one": "STRING"
#         "two": "STRING"
#         "three": "STRING"
#       }
#     }
def schema:
  def object($v): ( $v | with_entries(.value |= schema) | simplify_schema_obj );
  def array($v): ( [ $v[0] | schema ] );
  def string($v):    ( "STRING" );
  def number($v):    ( "NUMBER" );
  def boolean($v):   ( "BOOLEAN" );

 . | if   type == "object"  then object(.) 
     elif type == "array"   then array(.)
     elif type == "string"  then string(.)
     elif type == "number"  then number(.)
     elif type == "boolean" then boolean(.)
 else empty end;

# Converts human readable byte string like `12.34M` to a number
#
# Example for sorting
#   jq '. | sort_by(.Field | to_bytes)'
def to_bytes: 
  capture("^(?<num>[0-9.]+)(?<unit>[KMGT]?)$") as $c | 
    $c.num | tonumber * (if $c.unit == "K" then 1024 
                        elif $c.unit == "M" then 1024*1024 
                        elif $c.unit == "G" then 1024*1024*1024 
                        else 1 end);
