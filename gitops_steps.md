
We specify if the method is generic (valid for all application) or specific to a language/framework/project type (e.g. Shiny)





# Setup Dockerhub [generic]
- create repo

# Setup Github [generic]
- tokens
- 



# Setup project
## Add deployment files [generic]
- un pipeline de CI (.github/workflows/ci.yml) qui rebuild l'image à chaque modif du code source directement sur le cluster SSPCloud (via kaniko) et l'envoie sur le registry docker associé à git.lab.sspcloud.fr





- dans le dossier deployment, trois fichiers qui créent respectivement les ressources Kube nécessaires au déploiement d'une app (deployment, service et ingress)
- un fichier GitOps (argocd-deploy.yaml) qui automatise le redéploiement avec argo-cd en cas de modif de la config (en toute rigueur ces fichiers de déploiement devraient être dans un repo indépendant, pour découpler le build de l'image et le déploiement, mais bon pour l'exemple c'est pas un problème)

## Create Dockerfile
### With R tooks
using rocker
using golem (which uses rocker) golem::add_dockerfile() or golem::add_dockerfile_shinyproxy()


# Build dockerfile

kubectl apply -f argocd-deploy.yaml