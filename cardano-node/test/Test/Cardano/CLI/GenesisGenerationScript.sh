
echo $(dirname $0)

. $(dirname $0)/Core/runner.sh

# Variables

testname="Genesis Generation"
genesisjsonfile="/home/jordan/Repos/Work/userepotool/cardano-node/configuration/genesis/genesis.json"
genesisdir=$(dirname "${genesisjsonfile}")
error=0

banner "${testname}"

# Begin test

./scripts/genesis.sh
wait

assert_file_exists ${genesisjsonfile}

if test "${error}" = "0"; then
	pass_test "${testname}"
else
	fail_test
fi
