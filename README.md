# RefitControllerGenerator

Анализатор кода для автоматической генерации ASP.NET Core контроллеров из Refit-интерфейсов.

## Сборка анализатора

Для сборки анализатора выполните команду:

```bash
dotnet pack -c Debug
```
В папке RefitControllerGenerator/bin/Debug генерируется пакет с разрешением .nupkg

В проекте WebAPI можно добавить Source Package и прописать путь до папки RefitControllerGenerator/bin/Debug или выполнить команду

```bash
dotnet add package RefitControllerGenerator --source //путь до папки RefitControllerGenerator/bin/Debug
```

## Запуск анализатора

После установки Nuget-пакета следует переоткрыть окно студии, иначе анализатор не применяется.

Применение анализатора выполняется нажатием ctrl+. на типе рефит-описания в строке ``private readonly IKASKOPolicyApi kASKOPolicyService;`` в любой части проекта.

Выбрать ``Generate controller 'ControllerName'``. 

Контроллер генерируется в папку ``$"{document.Project.Name}.Controllers"``.
