.DEFAULT_GOAL 	:= help

build: ## Build project
	@echo "Compiling..." && ghc -o bin/hidato ./Main.hs src/Structures.hs src/Game.hs src/Generator.hs

run: ## Run project
	@bin/hidato

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
