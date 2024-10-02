lua <<EOF
require('crates').setup {
    text = {
        loading = "  Loading...",
        version = "  ✓ %s",
        prerelease = " * %s",
        yanked = "  x yanked",
        nomatch = "  Not found",
        upgrade = "  ↑ %s",
        error = "  Error fetching crate",
    },
    popup = {
        text = {
            title = "# %s",
            pill_left = "",
            pill_right = "",
            created_label = "created        ",
            updated_label = "updated        ",
            downloads_label = "downloads      ",
            homepage_label = "homepage       ",
            repository_label = "repository     ",
            documentation_label = "documentation  ",
            crates_io_label = "crates.io      ",
            categories_label = "categories     ",
            keywords_label = "keywords       ",
            version = "%s",
            prerelease = "%s pre-release",
            yanked = "%s yanked",
            enabled = "* s",
            transitive = "~ s",
            normal_dependencies_title = "  Dependencies",
            build_dependencies_title = "  Build dependencies",
            dev_dependencies_title = "  Dev dependencies",
            optional = "? %s",
            loading = " ...",
        },
    },
    completion = {
        text = {
            prerelease = " pre-release ",
            yanked = " yanked ",
        },
    },
}
EOF

