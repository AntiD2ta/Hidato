.DEFAULT_GOAL 	:= help

build: ## Build project
	@echo "Compiling..." && ghc --make main 

run: ## Run project
	@./main 

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
