# Internal namespace aliases for functions intentionally called unqualified in
# dynamically generated ggplot aesthetics. Keeping the alias internal avoids a
# new public API while making package-namespace lookup deterministic.
interaction <- base::interaction
