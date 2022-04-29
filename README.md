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

## Tabla de compradores

Más que una dependencia es que hay un proceso importante en la aplicación que son los compradores. Estos compradores salen de una tabla Z propía del cliente que no estará en el sistema donde se ponga. Hay que tenerlo en cuenta porque será otro motivo por el cual dará error al activar la aplicación.

# Definición y funcionamiento general

## Definición

El primer nivel de la aplicación es la entidad: departamento/filial. En esta entidad se agrupan los los grupos de compras donde el usuario es aprobador o comprador.

Los grupos de compra se recuperan del usuario mirando:
1. En que estrategias es aprobador
2. En que grupos de compra esta asignado como comprador

Un usuario podrá ver los grupos de compras asignados a un departamento, aunque no sea aprobador ni comprador, si esta asignado en una tabla de autorizaciones. Ya que la asignación de grupos de compras a departamentos/filiales se hace en una tabla a medida propia de la aplicación.

# Funcionamiento

Los pasos que realiza la aplicación al entrar son los siguientes:

1. Obtener los departamentos del usuario para ello:
  2. Mira en que grupos de compras es comprador
  3. Mira en que grupos de compra es aprobador 
  4. 
  
4. De esos grupos se mira en que departamentos están asignados

# Menú de la aplicación

El menú principal de configuración es el *ZREL_STRAG* en esta menú se encontrará tanto las opciones de configuración como utilidades que se han creado para el testeo de la aplicación:

![menu](https://github.com/irodrigob/ABAP_Release_strategy/blob/main/docs/menu_ambito_aplicacion.png)

# Configuración

Las estrategias se agrupan en grupos de compras y estos en departamentos. Para que la aplicación en frontend funcione hay que hacer los siguientes pasos:

1. Definir los departamentos
2. Asociar los grupos de compras a departamentos
3. Asociar los usuarios a los departamentos

Cosas que propias de cliente para el cual se ha desarrollado esta aplicación:

* El cliente tiene una tabla Z donde 

