{
    "name": "sober",
    "title": "Трезвый водитель",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "fromAddress",
            "groupName": "address",
            "meta": {
                "label": "Где забрать"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "toAddress",
            "groupName": "address",
            "meta": {
                "label": "Куда доставить"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "multidrive",
            "meta": {
                "label": "Мультидрайв"
            },
            "type": "checkbox",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "status",
            "canWrite": true,
            "canRead": true,
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        }
    ]
}
