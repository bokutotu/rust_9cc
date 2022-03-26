default:
	cargo clean
	cargo build

test:
	cargo clean
	cargo build
	cargo fmt
	cargo clippy
	sh test_script/test.sh

commit:
	cargo clean
	cargo fmt
	cargo clippy
	sh test_script/test.sh
	git add .
	git commit
