#-n - name
#-p - parent id
#-c - credentials

#example curl/Category/create.sh -n CurlCateg -p 3 -c admin:admin  

while getopts ":n:p:c:" opt; do
  case $opt in
    n) name="$OPTARG"
    ;;
    p) parent="$OPTARG"
    ;;
    c) cred="$OPTARG"
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    exit 1
    ;;
  esac

  case $OPTARG in
    -*) echo "Option $opt needs a valid argument"
    exit 1
    ;;
  esac
done

echo $name

curl -v --request PUT -G \
  --url 'http://localhost:3000/category/create' \
  --data-urlencode "name=${name}" \
  --data-urlencode "parent=${parent}" \
  --header "Authorization: Basic `echo -n $cred | base64`"
  

# curl -v --request PUT \
#   --url "http://localhost:3000/category/create?name=${name}&parent=${parent}" \
#   --header "Authorization: Basic `echo -n $cred | base64`"