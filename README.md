# Elfos-tee

The tee utility runs an Elf/OS command while copying it's output into a file. Usage is:

> tee [-a] logfile command ...

This will run the command given and copy the output into a file named logfile. If the -a option is provided, it will append the output to an existing file. For example:

> Ready  
> : tee out ver tee  
> 8/11/2021 Build: 0  See github.com/dmadole/Elfos-tee for more info  
>  
> Ready  
> : type out  
> 8/11/2021 Build: 0  See github.com/dmadole/Elfos-tee for more info  

Tee requires kernel 0.4.0 and should work with most any program including shell and those run under shell.

