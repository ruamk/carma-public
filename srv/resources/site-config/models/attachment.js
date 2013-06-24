{
    "name": "attachment",
    "title": "Прикреплённый файл",
    "canCreate": false,
    "canRead": true,
    "canUpdate": false,
    "canDelete": true,
    "fields": [
        {
            "name": "filename",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "vwfake", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "file",
            "meta": {
                "label": "Файл"
            }
        }
    ]
}