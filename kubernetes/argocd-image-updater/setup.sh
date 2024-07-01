set -eux

function create_cluster() {
  kind create cluster
  kubectl cluster-info --context kind-kind
}

function install_argocd() {
  kubectl create namespace argocd
  kubectl apply -n argocd -f https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml
}

function install_argocd() {
  kubectl create namespace argocd
  kubectl apply -n argocd -f https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml
}

function install_argocd_image_updater() {
  kubectl apply -n argocd -f https://raw.githubusercontent.com/argoproj-labs/argocd-image-updater/stable/manifests/install.yaml
}

function install_old_nginx() {
  kubectl apply -f old-nginx/old-deployment.yaml
  kubectl apply -f old-nginx/old-service.yaml
}

# create_cluster
# install_argocd
# install_argocd_image_updater
install_old_nginx
