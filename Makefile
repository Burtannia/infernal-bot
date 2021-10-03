run:
	stack run -- --config "config.json"

build:
	stack build --copy-bins --local-bin-path .

deploy:
	stack build --copy-bins --local-bin-path .
	scp infernal-bot-exe james@jamesburton.dev:/opt/infernal-bot/incoming/infernal-bot-exe
	cat scripts/remote-deploy.sh | ssh james@jamesburton.dev