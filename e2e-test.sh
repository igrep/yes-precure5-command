#!/bin/sh

set -eu

stack exec yes precure 5 | head -n 1 | grep 'プリキュア！メタモルフォーゼ！' --quiet
