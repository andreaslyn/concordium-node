pipeline {
    agent any

    environment {
        build_profile = 'release'
        consensus_profiling = 'false'

        ecr_repo_base = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium'
        universal_image_repo = 'concordium/universal'
        universal_image_name = "${universal_image_repo}:${image_tag}"
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

        stage('build-universal') {
            steps {
                sh '''\
                    docker build \
                      --build-arg base_image_tag="${base_image_tag}" \
                      --build-arg static_libraries_image_tag="${static_libraries_image_tag}" \
                      --build-arg ghc_version="${ghc_version}" \
                      --build-arg consensus_profiling="${consensus_profiling}" \
                      --label base_image_tag="${base_image_tag}" \
                      --label static_libraries_image_tag="${static_libraries_image_tag}" \
                      --label ghc_version="${ghc_version}" \
                      --label consensus_profiling="${consensus_profiling}" \
                      -t "${universal_image_name}" \
                      -f ./scripts/node/universal.Dockerfile \
                      .
                '''
            }
        }
        stage('build-bootstrapper') {
            environment {
                image_repo = "${ecr_repo_base}/bootstrapper"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_profile="${build_profile}" \
                      --label universal_image_name="${universal_image_name}" \
                      --label build_profile="${build_profile}" \
                      -t "${image_name}" \
                      -f scripts/node/bootstrapper.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }

        stage('build-node') {
            environment {
                image_repo = "${ecr_repo_base}/node"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        docker build \
                          --build-arg universal_image_name="${universal_image_name}" \
                          --build-arg build_profile="${build_profile}" \
                          --label universal_image_name="${universal_image_name}" \
                          --label build_profile="${build_profile}" \
                          -t "${image_name}" \
                          -f scripts/node/node.Dockerfile \
                          .
                        docker push "${image_name}"
                    '''
                }
            }
        }

        stage('build-collector') {
            environment {
                image_repo = "${ecr_repo_base}/node-collector"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_profile="${build_profile}" \
                      --label universal_image_name="${universal_image_name}" \
                      --label build_profile="${build_profile}" \
                      -t "${image_name}" \
                      -f scripts/node/node-collector.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
