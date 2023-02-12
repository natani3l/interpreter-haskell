# Execute all files in directory /examples 
# - chmod +x main.sh
# ./main.sh

for FILE in examples/*/*; do echo $FILE; runghc Main.hs < $FILE; done