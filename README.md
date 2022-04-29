# Introducción

Este proyecto nace de la necesidad de tener una herramienta visual, para el proyecto realizada en UI5, que permita visualizar y mantener las estrategías de liberación de compras. 

# Requisitos ABAP

La aplicación ha sido desarrollada en una versión 7.5 de ABAP pero el requisito mínimo sería una 7.4. 

# Dependencias

## Motor de workflow

Esta aplicación tiene un proceso de aprobación de los cambios de estrategías, para ello hace uso de la aplicación [ABAP Workflow engine](https://github.com/irodrigob/ABAP_Workflow-Engine) para la gestión del proceso de aprobación.

## Plantilla envio mails

En el cliente donde se desarrollo el proyecto el proceso de plantillas y envio de mails es una versión antigua de los siguientes proyectos:

* [Envio de mail](https://github.com/irodrigob/ABAP_Mail_Utility)
* [Text templates](https://github.com/irodrigob/ABAP_Text_Templates)

Hay que tener claro que habrá que migrar el código existente por el nuevo sistema de envio de mails. En cliente donde se desarrollo no es posible hacerlo, por lo tanto, hay que dar por hecho que dará error al activar los objetos.

# Menú de la aplicación

El menú principal de configuración es el *ZREL_STRAG* en esta menú se encontrará tanto las opciones de configuración como utilidades que se han creado para el testeo de la aplicación:

![menu](https://github.com/irodrigob/ABAP_Release_strategy/blob/main/docs/menu_ambito_aplicacion.png)
