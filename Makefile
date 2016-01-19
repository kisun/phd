default:
	# Install apt dependencies:
	sudo apt-get update && sudo apt-get -y install libssl-dev libcurl4-openssl-dev pkg-config libssh2-1-dev libxml2-dev

	# install R packages
	R -f .Rlibrary/install.R
