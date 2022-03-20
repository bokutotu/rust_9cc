default:
	cargo build --release

test:
	cargo fmt
	cargo clippy
	sh test_script/test.sh

commit:
	cargo fmt
	cargo clippy
	sh test_script/test.sh
	git add .
	git commit
