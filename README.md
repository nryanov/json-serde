# json-serde

Json grammar source: https://github.com/antlr/grammars-v4/blob/master/json/JSON.g4

## Test
```shell script
sbt test
```

## Benchmark
```shell script
sbt becnh:test
```

## Benchmark results
```markdown
cores: 4
hostname: MAC
name: OpenJDK 64-Bit Server VM
osArch: x86_64
osName: Mac OS X
vendor: Oracle Corporation
version: 11.0.2+9
```

### json-serde
```markdown
::Benchmark jsonserde.serialize::
Parameters(size -> 100000): 106.39798 ms

::Benchmark jsonserde.deserialize::
Parameters(size -> 100000): 7185.609219 ms
```

### circe
```markdown
::Benchmark circe.serialize::
Parameters(size -> 100000): 177.150326 ms

::Benchmark circe.deserialize::
Parameters(size -> 100000): 4.534527 ms
```