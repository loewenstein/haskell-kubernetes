#!/usr/bin/env bash

# grab all haskell files from initial temp dir.
all_files() {
  local tmpdir="${1}"
  find "${tmpdir}" -name '*.hs'
}

# Get all files for a prefix
get_prefixed_files() {
  local prefix="${1}"
  find "./lib/Kubernetes/Model/${prefix}/" -name '*.hs'
}

# grab all V1 files.
v1_files() {
  get_prefixed_files "V1"
}

# grab all Unversioned files.
unversioned_files() {
  get_prefixed_files "Unversioned"
}

json_files() {
  get_prefixed_files "Json"
}

# Calculate new filename, removing a prefix.
new_unprefixed_name() {
  local prefix="${1}"
  local filename="${2}"

  basename "${filename}" | sed 's/'"${prefix}"'\.//g'
}

# move files of a certain prefix to their prefixed directory
move_prefixed_files_io() {
  local prefix="${1}"
  local tmpdir="${2}"
  for filename in $(all_files "${tmpdir}" | grep "${prefix}\.")
  do
    cp "${filename}" "./lib/Kubernetes/Model/${prefix}/$(new_unprefixed_name "${prefix}" "${filename}")"
  done
}

move_v1_io() {
  local tmpdir="${1}"
  move_prefixed_files_io "V1" "${tmpdir}"
}

# move unversioned-prefixed files.
move_unversioned_io() {
  local tmpdir="${1}"
  move_prefixed_files_io "Unversioned" "${tmpdir}"
}

# move json-prefixed files
move_json_io() {
  local tmpdir="${1}"
  move_prefixed_files_io "Json" "${tmpdir}"
}

# code gen results in:
# import Model.Foo (Model.Foo)
# but we want:
# import Model.Foo (Foo)
remove_import_model_io() {
  for file in $(v1_files) $(unversioned_files) $(json_files)
  do
    sed -i 's/(Model\./(/g' "${file}"
  done
}

clean_prefixes_io() {

  local file_list="${1}"

  #for file in $(v1_files) $(unversioned_files) $(json_files)
  for file in $file_list
  do
    # sanitize: move Kubernetes.V1.Model. to KubernetesModelV1
    for prefix in "V1" "Unversioned" "Json"
    do
      sed -i 's/Kubernetes\.'"${prefix}"'\.Model/Kubernetes'"${prefix}"'Model/g' "${file}"
    done
    # rename V1.xxx -> xxx
    for prefix in "V1" "Unversioned" "Json"
    do
      sed -i 's/'"${prefix}"'\.//g' "${file}"
    done
    # restore sanitized names.
    for prefix in "V1" "Unversioned" "Json"
    do
      sed -i 's/Kubernetes'"${prefix}"'Model/Kubernetes.'"${prefix}"'.Model/g' "${file}"
    done
    # rename Kubernetes.V1.Model to Kubernetes.Model.V1
    for prefix in "V1" "Unversioned" "Json"
    do
      sed -i 's/Kubernetes\.'"${prefix}"'.Model/Kubernetes.Model.'"${prefix}"'/g' "${file}"
    done
  done

}

# Json files get improted with an erroneous module prefix.
remove_prefixed_module_io() {
  local prefix="${1}"
  local file_list="${2}"

  for file in $file_list
  do
    sed -i 's/module Kubernetes\.Model\.V1/module Kubernetes.Model.'"${prefix}"'/g' "${file}"
  done
}

# fix unversioned and json imports
repair_module_io() {
  local prefix="${1}"
  local files_to_repair="${2}"

  for broken_model in $(find "./lib/Kubernetes/Model/${prefix}" -name '*.hs')
  do
    local model="$(basename "${broken_model}" ".hs")"
    for file in $files_to_repair
    do
      sed -i 's/V1\.'"${model}"'/'"${prefix}"'.'"${model}"'/g' "${file}"
    done
  done
}

# remove redundant Data.Text import
remove_text_import_io() {
  local file_list="$(ack --haskell -L ':: ((Maybe\s)?\[?Text\]?)' ./lib/Kubernetes/Model/)"

  for file in $file_list
  do
    sed -i '/Data\.Text/d' "${file}"
  done
}

# one off weird fixes
one_off_io() {
  sed -i 's/Port/port/g' ./lib/Kubernetes/Model/V1/DaemonEndpoint.hs
}

main() {

  local tmpdir="${1}"

  rm ./lib/Kubernetes/Model/V1/*
  rm ./lib/Kubernetes/Model/Json/*
  rm ./lib/Kubernetes/Model/Unversioned/*

  echo "moving files..."
  move_v1_io "${tmpdir}"
  move_unversioned_io "${tmpdir}"
  move_json_io "${tmpdir}"
  echo "... done!"

  echo "removing import weirdness..."
  remove_import_model_io
  echo "... done!"

  echo "cleaning prefixes"
  clean_prefixes_io "$(v1_files) $(json_files) $(unversioned_files)"
  echo "... done!"

  echo "cleaning json modules"
  remove_prefixed_module_io "Json" "$(json_files)"
  echo "... done!"

  echo "cleaning unversioned modules"
  remove_prefixed_module_io "Unversioned" "$(unversioned_files)"
  echo "... done!"

  echo "Repair broken module imports"
  repair_module_io "Unversioned" "$(v1_files) $(json_files) $(unversioned_files)"
  repair_module_io "Json" "$(v1_files) $(json_files) $(unversioned_files)"
  echo "... done!"

  echo "Removing unnecessary Data.Text imports"
  remove_text_import_io
  echo "... done!"

  echo "perform one offs"
  one_off_io
  echo "... done!"

  echo "move Any.hs back"
  cp codegen/Any.hs ./lib/Kubernetes/Model/V1/Any.hs
  echo "... done!"


}
main $@
