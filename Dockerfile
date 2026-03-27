# Stage 1: Build
FROM ocaml/opam:debian-12-ocaml-5.3 AS builder

WORKDIR /home/opam/app

# Install system dependencies
RUN sudo apt-get update && sudo apt-get install -y \
    pkg-config \
    libev-dev \
    libssl-dev \
    libgmp-dev \
    && sudo rm -rf /var/lib/apt/lists/*

# Install OCaml dependencies
RUN opam install dream yojson dune --yes

# Copy project files
COPY --chown=opam:opam dune-project grammar_forge.opam ./
COPY --chown=opam:opam lib/ lib/
COPY --chown=opam:opam bin/ bin/
COPY --chown=opam:opam test/ test/

# Build
RUN eval $(opam env) && dune build --release

# Stage 2: Runtime
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    libev4 \
    libssl3 \
    libgmp10 \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -r grammarforge && useradd -r -g grammarforge -m grammarforge

WORKDIR /app

# Copy binary from builder
COPY --from=builder /home/opam/app/_build/default/bin/main.exe /app/grammar-forge

RUN chown -R grammarforge:grammarforge /app

USER grammarforge

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

CMD ["/app/grammar-forge"]
