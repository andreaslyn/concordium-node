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
    }
}
