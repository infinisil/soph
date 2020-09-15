# Give your project a name. :)
workspace(name = "soph")

## Download and load rules_haskell
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.12",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.12.tar.gz"],
    sha256 = "56a8e6337df8802f1e0e7d2b3d12d12d5d96c929c8daecccc5738a0f41d9c1e4",
)
load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Configure rules_haskell
rules_haskell_dependencies()
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_python_configure",
)
nixpkgs_python_configure(
    repository = "@nixpkgs",
)

# Get a nixpkgs for some dependencies. Not sure what this is exactly used for actually
nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "800184df21cfe4924e1e901f387cda49a9a8269c",
    sha256 = "5107e494c16d56faa3c8c1a1e2db21d4dd9143a840185f9d120e3f51adf55256",
)
load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)
haskell_register_ghc_nixpkgs(
    version = "8.4.4",
    nix_file = "//:default.nix",
    attribute_path = "ghc",
    nix_file_deps = [ "//:soph.cabal" ],
    repository = "@nixpkgs",
)

