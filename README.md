# CU-OpenStack

Cu-openstack is a web application/middleware that makes it easy to
create (and delete) ready-to-use virtual machine instants on an
OpenStack cluster. It is intended for internal use at Chulalongkorn
University (hence the name and the branding), but, because it
interfaces with OpenStack using the REST API, it will also work with
any OpenStack cluster.

**Warning:** Currently, cu-openstack is in a pre-alpha stage. Expect lots
of bugs, breakages, and missing features.

## Getting Started

Start by initializing a configuration file by invoking the following function:

````lisp
CU-OPENSTACK> (models:init-config)
````

This will create a configuration file named config.json.

Edit the configuration file and supply it with the host of the
Keystone service and a username and password of an OpenStack user that
has the appropriate permission, then initialize the database and start
the server using the following commands:

````lisp
CU-OPENSTACK> (models:init-db)
CU-OPENSTACK> (start-easy-server 8080)
````

And points your browser at http://localhost:8080 (assuming you are
using the same port as the example above).

Note that cu-openstack requires an access to a running instant of an
Openstack cluster in order to function properly (DevStack will also do
just fine for testing/development purposes).

## Dependencies

* **trivial-openstack**
* **hunchentoot**
* **cl-who**
* **cl-pass**
* **datafly**
* **uri-template**

## License

Copyright (c) 2016 Smith Dhumbumroong

Licensed under the MIT License.
