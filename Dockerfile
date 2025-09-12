FROM gleamlang/gleam:latest

WORKDIR /app

COPY gleam.toml ./
COPY src ./src
COPY test ./test

RUN gleam build

CMD ["gleam", "run"]