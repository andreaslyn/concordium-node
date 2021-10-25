@Library('concordium-pipelines') _
pipeline {
    agent any
    environment {
        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
    }
    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
            }
        }
        stage('build-genesis') {
            environment {
                image_repo = "${ecr_repo_domain}/concordium/genesis"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        # Using '--no-cache' because we're cloning genesis data
                        # and BuildKit (and '--ssh default') because the repo is on GitLab.
                        # Using '--pull' to ensure that we build from the latest Alpine base images.
                        DOCKER_BUILDKIT=1 docker build \
                          --ssh default \
                          --no-cache \
                          --pull \
                          --build-arg=tag=${tag} \
                          --build-arg=path=${path} \
                          --label=tag=${tag} \
                          --label=path=${path} \
                          -t "${image_name}" \
                          -f scripts/node/genesis.Dockerfile \
                          .
                        docker push "${image_name}"
                    '''
                }
            }
        }
    }
}
