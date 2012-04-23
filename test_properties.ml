open Batteries_uni

open OUnit
open Printf
open TUtil

let sample1 = "\
<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
<!DOCTYPE properties SYSTEM \"http://java.sun.com/dtd/properties.dtd\">\n\
<properties>\n\
<comment>Backup</comment>\n\
<entry key=\"_date\">1331354632558</entry>\n\
<entry key=\"kind\">snapshot</entry>\n\
<entry key=\"hash\">f4d4a18a53bb4c779c7544642a03b36d8f5226c7</entry>\n\
<entry key=\"test\">foo</entry>\n\
</properties>\n"

let prop1 = Map.StringMap.of_enum **> List.enum [
  "_date", "1331354632558";
  "kind", "snapshot";
  "hash", "f4d4a18a53bb4c779c7544642a03b36d8f5226c7";
  "test", "foo" ]

let java () =
  let table = Properties.of_java_xml sample1 in
  assert_equal prop1 table

let suite = "properties" >::: [
  "java" >:: java;
]
