#-i - category id
#-n? - new name
#-p? - new parent
#-c - credentials

#example curl/Category/alter.sh -i 10 -n CurlCateg -p 4 -c admin:admin

while getopts ":i:n:p:c:" opt; do
  case $opt in
    i) id="$OPTARG"
    ;;
    p) parent="$OPTARG"
    ;;
    n) name="$OPTARG"
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

if [ ! -z $parent ] 
then 
    par="parent=${parent}"
else
    par=""
fi
echo $name

if [ ! -z "${name}" ] 
then 
    nam="name=${name}"
else
    nam="" 
fi

echo $nam
echo $par

curl -v --request POST -G \
  --url "http://localhost:3000/category/alter/${id}" \
  --data-urlencode "$par" \
  --data-urlencode "$nam" \
  --header "Authorization: Basic `echo -n $cred | base64`" 