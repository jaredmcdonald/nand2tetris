#!/bin/bash
cargo test

cargo run ../10/Square/SquareGame.jack > ../10/Square/SquareGame-generated.xml
../../tools/TextComparer.sh ../10/Square/SquareGame.xml ../10/Square/SquareGame-generated.xml


cargo run ../10/Square/Square.jack > ../10/Square/Square-generated.xml
../../tools/TextComparer.sh ../10/Square/Square.xml ../10/Square/Square-generated.xml

cargo run ../10/Square/Main.jack > ../10/Square/Main-generated.xml
../../tools/TextComparer.sh ../10/Square/Main.xml ../10/Square/Main-generated.xml
