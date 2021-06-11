pipeline {
    agent any

    environment {
        ecr_repo_base = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium'
    }

    stages {
        stage('ecr-login') {
            steps {
                sh 'aws ecr get-login-password \
                        --region eu-west-1 \
                    | docker login \
                        --username AWS \
                        --password-stdin 192549843005.dkr.ecr.eu-west-1.amazonaws.com'
            }
        }

        stage('build-genesis') {
            environment {
                image_repo = "${ecr_repo_base}/genesis"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        # Using '--no-cache' because we're cloning genesis data
                        # and BuildKit (and '--ssh default') because the repo is on GitLab.
                        DOCKER_BUILDKIT=1 docker build \
                          --build-arg genesis_ref="${genesis_ref}" \
                          --build-arg genesis_path="${genesis_path}" \
                          --label genesis_ref="${genesis_ref}" \
                          --label genesis_path="${genesis_path}" \
                          -t "${image_name}" \
                          -f scripts/testnet-deployments/genesis.Dockerfile \
                          --ssh default \
                          --no-cache \
                          .
                        docker push "${image_name}"
                    '''
                }
            }
        }
    }
}
