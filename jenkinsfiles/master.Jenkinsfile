@Library('concordium-pipelines') _
pipeline {
    agent any
    environment {
        ecr_repo_base = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium'
        universal_image_repo = 'concordium/universal'
        universal_image_name = "${universal_image_repo}:${image_tag}"
        build_profile = 'release'
    }
    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
            }
        }
        stage('build-genesis') {
            environment {
                image_repo = "${ecr_repo_base}/node-genesis"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        # Using '--no-cache' because we're cloning genesis data
                        # and BuildKit (and '--ssh default') because the repo is on GitLab.
                        DOCKER_BUILDKIT=1 docker build \
                          --ssh default \
                          --no-cache \
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
        stage('build-universal') {
            steps {
                sh '''\
                    docker build \
                      --build-arg base_image_tag="${base_image_tag}" \
                      --build-arg static_libraries_image_tag="${static_libraries_image_tag}" \
                      --build-arg ghc_version="${ghc_version}" \
                      --label base_image_tag="${base_image_tag}" \
                      --label static_libraries_image_tag="${static_libraries_image_tag}" \
                      --label ghc_version="${ghc_version}" \
                      -t "${universal_image_name}" \
                      -f ./scripts/node/universal.Dockerfile \
                      -- pull \
                      .
                '''
            }
        }
        stage('build-bootstrapper') {
            environment {
                image_repo = "${ecr_repo_domain}/concordium/bootstrapper"
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
                image_repo = "${ecr_repo_domain}/concordium/node"
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
                      -f scripts/node/node.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
        stage('build-collector') {
            environment {
                image_repo = "${ecr_repo_domain}/concordium/node-collector"
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
