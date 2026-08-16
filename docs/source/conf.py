import os
import subprocess
# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'MCPSCU Documents'
copyright = '2017-2026, Sichuan university'
author = 'Lei Zhai(zhailei-scu), Qing-Hou'

release = "latest"

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "myst_parser",
    "sphinxcontrib.mermaid",
]

templates_path = ['_templates']
exclude_patterns = []



# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

#html_theme = 'alabaster'
html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']

html_context = {}

html_context["display_manual_versions"] = True

html_context["current_version"] = "release"

html_context["versions"] = [
    (
        "develop",
        "/develop/"
    ),
    (
        "stable",
        "/stable/"
    ),
    (
        "release",
        "/"
    ),
]

html_context["downloads"] = [
    (
        "PDF",
        "downloads/MCPSCU.pdf"
    ),
]

try:
    git_commit = subprocess.check_output(
        [
            "git",
            "rev-parse",
            "--short",
            "HEAD"
        ]

    ).decode().strip()
except:
    git_commit="unknown"

html_context["git_commit"] = git_commit

html_context["release"] = "release"

myst_enable_extensions = [
    "amsmath",

    "colon_fence",

    "deflist",

    "html_image",
]

copybutton_prompt_text = r">>> |\.\.\. "

mermaid_version = "10.6.1"