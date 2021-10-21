pipeline {
    agent any

    environment {
        build_profile = 'release'

        ecr_repo_base = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium'
        image_repo = "${ecr_repo_base}/collector-backend"
        image_name = "${image_repo}:${image_tag}"
    }

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }

        stage('build-collector-backend') {
            steps {
                sh '''\
                    docker build \
                      --build-arg base_image_tag="${base_image_tag}" \
                      --build-arg build_profile="${build_profile}" \
                      --label base_image_tag="${base_image_tag}" \
                      --label build_profile="${build_profile}" \
                      -t "${image_name}" \
                      -f scripts/node/collector-backend.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
