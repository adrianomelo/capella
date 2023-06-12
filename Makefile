serve:
	cabal run -- capella-exe

docker:
	docker buildx build --platform linux/arm64 -t adrianomelo/capella:latest --push .