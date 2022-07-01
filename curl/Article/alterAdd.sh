#!/bin/bash

#-d - article to be altered
#-t? - title
#-n? - content
#-i? - images (Pathes with ';' as a separator)
#-p? - is published
#-y? - category id
#-c - credentials

#NO SPACES IN FILEPATHS

#example: curl/Article/alterAdd.sh -c admin:admin -d 72 -t "Новый тайтл с пробелами" -p False -y 1

while getopts ":d:t:n:i:p:y:c:" opt; do
  case $opt in
    d) id="$OPTARG"
    ;;
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

if [ ! -z "${title}" ] 
then 
    titleForm=(--form "title=$title")
else
    titleForm=()
fi


if [ ! -z "${content}" ] 
then 
    contentForm=(--form "content=$content")
else
    contentForm=() 
fi

echo $contentForm

if [ ! -z ${isPublished} ] 
then 
    isPublishedForm=(--form "is_published=$isPublished")
else
    isPublishedForm=() 
fi

if [ ! -z ${category} ] 
then 
    categoryForm=(--form "category_id=$category")
else
    categoryForm=()
fi

index=0
imageForms=""
imagesAr=$(echo $images | tr ";" "\n")

for image in $imagesAr
do
  imageForms="$imageForms -F file${index}=@${image}"  
  index=$(($index + 1))
done

curl -v --request POST \
  --url http://localhost:3000/article/alter/${id} \
  --header "Authorization: Basic `echo -n $cred | base64`" \
  --header 'Content-Type: multipart/form-data' \
  --header 'content-type: multipart/form-data; boundary=---011000010111000001101001' \
  "${titleForm[@]}" \
  "${contentForm[@]}" \
  "${categoryForm[@]}" \
  "${isPublishedForm[@]}" \
  $imageForms