#!/bin/sh

$(stack path --local-install-root)/bin/main "$@" || echo "run error code: $?"
