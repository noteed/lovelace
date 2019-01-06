/var/www/tip/_site/images/lovelace.svg: example.svg
	cp example.svg /var/www/tip/_site/images/lovelace.svg

example.svg: example.dot
	nix-shell -p graphviz --run 'dot -Tsvg -o example.svg example.dot'

example.dot: Lovelace.hs bin/lovelace.hs
	nix-shell --pure --run 'runghc bin/lovelace.hs graph'
