from setuptools import setup, find_packages


def get_package_info():
    with open("version.txt") as version_file:
        return (line.strip() for line in version_file.read().strip().split())


def get_requirements():
    with open('requirements.txt') as req_file:
        return [
            line.strip('\n')
            for line in req_file.readlines()
            if line and not line.startswith('#')
        ]


def setup_package():
    package_name, version = get_package_info()
    requirements = get_requirements()
    standard_kwargs = {
        'name': package_name,
        'description': 'Automated Testing for ORM Systems',
        'version': version,
        'packages': find_packages(),
        'install_requires': requirements,
        'scripts': ['cynthia/cynthia']
    }
    setup(**standard_kwargs)


setup_package()
