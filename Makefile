.DEFAULT_GOAL 	:= help

build: ## Build project
	@cabal build

run: ## Run project
	@cabal run

install: ## Install project
	@cabal install

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
