#-t - title
#-n - content
#-i - images  (Pathes with ';' as a separator)
#-p - is published
#-y - category id
#-c - credentials

#NO SPACES IN FILEPATH

#example: curl/Article/create.sh -t "Curl Title" -n "Curl Content" -i "curl/imagesForTesting/image2.jpg;curl/imagesForTesting/image1.jpg" -p False -y 1 -c Author:AuthorPass

while getopts ":t:n:i:p:y:c:" opt; do
  case $opt in
    t) title="$OPTARG"
    ;;
    n) content="$OPTARG"
    ;;
    i) images="$OPTARG"
    ;;
    p) isPublished="$OPTARG"
    ;;
    y) category="$OPTARG"
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

imagesAr=$(echo $images | tr ";" "\n")
index=0
imageForms=""

for image in $imagesAr
do
  imageForms="$imageForms -F file${index}=@${image}"  
  index=$(($index + 1))
done

curl -v --request PUT \
  --url http://localhost:3000/article/create \
  --header "Authorization: Basic `echo -n $cred | base64`" \
  --header 'Content-Type: multipart/form-data' \
  --header 'content-type: multipart/form-data; boundary=---011000010111000001101001' \
  --form "title=$title" \
  --form "content=$content" \
  --form is_published=$isPublished \
  --form category_id=$category \
  ${imageForms}