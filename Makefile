/var/www/tip/_site/images/example.svg: example.svg example-1.svg example-2.svg example-3.svg example-4.svg example-5.svg example-6.svg example-7.svg
	cp *.svg /var/www/tip/_site/images/

%.svg: %.dot
	nix-shell -p graphviz --run 'dot -Tsvg -o $@ $<'

example.dot: Lovelace.hs bin/lovelace.hs
	nix-shell --pure --run 'runghc bin/lovelace.hs graph'
